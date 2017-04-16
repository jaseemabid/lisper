{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Macro where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Maybe (catMaybes, mapMaybe, fromJust)

import Lisper.Core
import Lisper.Token

--
-- § Types
--

-- | Identifier is an optional value matched literally in a macro
--
-- For example, the following macro contains one identifier, @=>@
--
-- > (define-syntax bind
-- >   (syntax-rules (=>)
-- >     ((bind a => b) (b a))))
--
type Identifier = Scheme

-- | A pattern is a list that begins with the keyword for the macro.
--
-- A pattern is an identifier, a constant, or one of the following.
--
-- 1. (pattern ...)
-- 1. (pattern pattern ... . pattern)
--
-- For example, the following macro contains one pattern, @(bind a => b)@
--
-- > (define-syntax bind
-- >   (syntax-rules (=>)
-- >     ((bind a => b) (b a))))
--
type Pattern = Scheme

-- | Template is the result of a `Pattern` match
--
-- For example, the following macro contains one template, @(b a)@
--
-- > (define-syntax bind
-- >   (syntax-rules (=>)
-- >     ((bind a => b) (b a))))
--
type Template = Scheme

-- | Alias for the name of a macro
type Name = String

-- | A rule is a `Pattern` and a `Template` to rewrite to when matched
--
-- Consider the macro
--
-- > (define-syntax bind
-- >   (syntax-rules (=>)
-- >     ((bind a => b) (b a))))
--
-- The rule will be of the form @Rule (bind a => b) (b a)@
--
data Rule = Rule Pattern Template
  deriving (Eq, Show)

-- | A macro is a set of `Identifier`s and rewrite `Rule`s
data Macro = Macro Name [Identifier] [Rule]
  deriving (Eq, Show)

-- | A list of named macros, like `Env`, but at compile time
type Macros = [(Name, Macro)]

-- | A compiler is a state transformer that returns an AST
type Compiler a = StateT Macros (ExceptT String Identity) a

--
-- § Implementation
--

-- | Compile an expression; building or expanding macros
--
-- If a macro definition is encountered, extract it and remove it from source
-- (build). Expand macros when possible. Leave everything else as it is.
--
compile1 :: Scheme -> Compiler (Maybe Scheme)
compile1 (s@(List [Symbol "define-syntax", Symbol name, transformer])) =
    case build transformer of
        Right macro -> do
            modify $ \env -> (name, macro) : env
            return Nothing
        Left _err -> return $ Just s

compile1 (s@(List (Symbol name: _rest))) = do
    macros <- get
    return . pure $ maybe s (`expand` s) $ lookup name macros

compile1 expression = return $ Just expression

-- | Build a Macro object from AST
--
-- Input is an expression of the form @(syntax-rules ...)@
build :: Scheme -> Either String Macro
build (List (Symbol "syntax-rules": List identifiers: rules')) = do
    rules <- mapM alistToRule rules'
    name <- getName rules

    Right $ Macro name identifiers rules

  where
    getName :: [Rule] -> Either String Name
    getName (Rule (List (Symbol sym: _)) _template: _xs) = Right sym
    getName err = Left $ "Unable to fetch name for macro " ++ show err

    -- [TODO] - Ensure that all patterns starts with the macro name
    alistToRule :: Scheme -> Either String Rule
    alistToRule (List [a, b]) = Right $ Rule a b
    alistToRule r = Left $ "Unknown macro rule: " ++ show r

build expr = Left $ "Error build macro: " ++ show expr


-- | Expand a macro
--
-- Consider the macro
--
-- > (define-syntax bind
-- >   (syntax-rules (=>)
-- >     ((bind a => b) (b a))))
--
-- The usage @(bind #t => not)@ will get expanded to @(not #t)@
--
-- @(bind a => b)@ is the pattern and @(b a)@ is the template
--
-- == Implementation
--
-- 1. Zip pattern and expression to get rewrite env
--
-- 2. Walk through template and replace all symbols with values from this env if
--    available, else leave it as it is.
--
expand :: Macro -> Scheme -> Scheme
expand (Macro name ids (Rule pattn template: rules)) usage =
    if match ids pattn usage
    then fromJust $ replace template
    else expand (Macro name ids rules) usage

  where

    -- | Replace each symbol in template with mapping from rewrites
    --
    -- Ie, look for @a@ in @[(a, 1), (b, inc)]@, and replace if available
    --
    replace :: Scheme -> Maybe Scheme
    -- replace l@(List ((Symbol car):_xs))
    --   | car == name = Just $ expand m l
    --   | otherwise = replace l
    replace (List xs) = Just $ List $ mapMaybe replace xs
    replace (Symbol "...") = Nothing
    replace (Symbol a) =
        case lookup a rewrites of
            Just val -> Just val
            Nothing -> Just $ Symbol a

      where
        -- | Make an alist of items to replace
        --
        -- @(a b c ...) -> (1 2 3 4 5) -> {a: 1, b: 2, c: (3 4 5)}@
        rewrites :: [(String, Scheme)]
        rewrites =
            case (pattn, usage) of
                (List ps, List us) ->
                    catMaybes $ zipWith zipper ps us

                (_pattn, _usage) ->
                    -- A pattern is a list that begins with the macro keyword
                    error $ "Malformed pattern " ++ show a ++ " in macro " ++ name

    replace expression = Just expression

    zipper :: Scheme -> Scheme -> Maybe (String, Scheme)
    zipper (Symbol a) s
      -- Ignore macro name
      | a == name && (s == Symbol name) = Nothing

      -- Ignore identifiers
      | Symbol a `elem` ids = Nothing

      -- Variable binding, symbol to something else
      | a /= name = Just (a, s)

    zipper a b = error $ "Cant zip " ++ show a ++ " <> " ++ show b

expand m@(Macro name _identifiers _rules) _expr =
    error $ "Error compiling macro " ++ name ++ " " ++ show m


-- [TODO] - Handle identifiers in macros, this is too naive

-- | Check if a predicate will match an expression
--
-- See § 4.3.2; Pattern language
--
-- >>> match _ (bind a => b) (#t => not)
-- True
-- >>> match _ (bind a => b) (+ 1 1)
-- False
--
match :: [Identifier] -> Pattern -> Scheme -> Bool
match ids predicate usage =
    case (literalp, predicate, usage) of

        -- P is a non literal identifier;
        -- `a` in `(or a b)` will match `(or #t 1)`
        (False, Symbol _, _anything) -> True

        -- P is a literal identifier and F is an identifier with the same
        -- binding. `else` in (cond ...) or => needs an exact match.
        (True, Bool a, Bool b) -> a == b
        (True, Number a, Number b) -> a == b
        (True, Symbol a, Symbol b) -> a == b
        (_, List [], _) -> usage == nil

        -- Pattern variables that occur on subpatterns followed by one or more
        -- instances of the identifier ... are allowed only in subtemplates that
        -- are followed by as many as instances of ... . They are replaced in
        -- the output by all of the subforms they match in the input,
        -- distributed as indicated.
        --
        -- [TODO] - This is not correct, but a rough approximation
        (_, List (Ellipses: _xs), _) -> True

        (_, List(x: xs), List(y: ys)) ->
            match ids x y && match ids (List xs) (List ys)

        (a, b, c) -> error $ "Unknown match format" ++ show (a, b, c)
  where
    literalp = predicate `elem` ids

-- § Exposed API

-- | Compile the AST and return the result, along with recognized macros
--
-- This method is stateless and subsequent applications wont behave like a REPL.
compile :: [Scheme] -> Either String ([Scheme], Macros)
compile ast = runIdentity $ runExceptT $ runStateT compiled macros
  where
    compiled = catMaybes <$> mapM compile1 ast
    macros = []
