{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Macro where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Maybe (catMaybes)

import Lisper.Core

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
data Macro = Macro String [Identifier] [Rule]
  deriving (Eq, Show)

-- | A list of named macros, like `Env`, but at compile time
type Macros = [(String, Macro)]

-- | A compiler is a state transformer that returns an AST
type Compiler a = StateT Macros (ExceptT String Identity) a

--
-- § Implementation
--



-- | Compile an expression; building and expanding macros
--
-- Walk through the string; if a macro definition is encountered, extract it and
-- remove it from source. Expand macros when possible. Leave everything else as
-- it is.
--
--
-- Make a macro object from the AST
-- [TODO] - Replace `define-syntax` with Nothing, not nil
compile1 :: Scheme -> Compiler Scheme
compile1 (s@(List [Symbol "define-syntax", Symbol name, transformer])) =
    case build transformer of
        Just t -> do
            modify $ \env -> (name, t) : env
            return nil
        Nothing -> return s

  where
    -- | Compile a Macro object from AST
    build :: Scheme -> Maybe Macro
    build (List (Symbol "syntax-rules": List identifiers: rules')) =
        Just $ Macro name identifiers rules

      where
        rules = map alistToRule rules'

        -- [TODO] - Ensure that all patterns starts with the macro name
        alistToRule (List [a, b]) = Rule a b
        alistToRule _ = error "Unknown macro rule"

    build _ = Nothing


compile1 (s@(List (Symbol name: _rest))) = do
  macros <- get
  case lookup name macros of
    Just macro -> return $ expand macro s
    Nothing -> return s

compile1 expression = return expression


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
    then replace template
    else expand (Macro name ids rules) usage

  where

    -- | Replace each symbol in template with mapping from rewrites
    --
    -- Ie, look for @a@ in @[(a, 1), (b, inc)]@, and replace if available
    --
    replace :: Scheme -> Scheme
    replace (List xs) = List $ map replace xs
    replace (Symbol a) =
        case lookup a rewrites of
            Just val -> val
            -- [TODO] - Recurse here, will fail for macros like `and`
            Nothing -> Symbol a

      where
        -- | Make an alist of items to replace
        rewrites :: [(String, Scheme)]
        rewrites =
            case (pattn, usage) of
                -- (a b) (1 2) -> {a: 1, b: 2}
                (List ps, List us) ->
                    catMaybes $ zipWith zipper ps us

                (_pattn, _usage) ->
                    -- A pattern is a list that begins with the keyword for the macro.
                    error $ "Malformed pattern " ++ show a ++ " in macro " ++ name

    replace expression = expression

    zipper :: Scheme -> Scheme -> Maybe (String, Scheme)
    zipper (Symbol a) s
      -- Ignore macro name
      | a == name && (s == Symbol name) = Nothing
      -- Ignore identifiers
      | Symbol a `elem` ids = Nothing
      -- Varaible binding, symbol to something else
      | a /= name = Just (a, s)

    zipper a b = error $ "Cant zip " ++ show a ++ " <> " ++ show b

expand m@(Macro name _identifiers _rules) _expr =
    error $ "Error compiling macro " ++ name ++ " " ++ show m


-- [TODO] - Handle identifiers in macros, this is too naive

-- | Check if a predicate will match an expression
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

        -- Empty list will only match itself
        (_, List [], _) -> usage == nil

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
compile ast = runIdentity $ runExceptT $ runStateT (mapM compile1 ast) []
