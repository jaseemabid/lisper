{-# LANGUAGE PatternSynonyms   #-}

module Lisper.Macro where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Maybe

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
expand :: Macro -> Scheme -> Scheme
expand m@(Macro _name ids rules) usage@(List _) =

    -- Return the usage as it is if there is no rule, otherwise, expand first
    -- and replace and recurse
    case listToMaybe $ filter f rules of
        Just rule -> expand m $ replace (rewrites ids rule usage) usage
        Nothing -> usage
      where
        f (Rule pattn _template) = match ids pattn usage

-- All non list macro expansion is no-op
expand _macro usage = usage

-- | Replace each symbol in template with mapping from rewrites
--
-- Ie, look for @a@ in @[(a, 1), (b, inc)]@, and replace if available
--
replace :: [(String, Scheme)] -> Scheme -> Scheme
replace rewrite usage =
    case usage of
        (List xs) -> List $ mapMaybe replace' xs
        _ -> usage

  where
    replace' Ellipses = Nothing
    replace' (Symbol a) =
        case lookup a rewrite of
            Just val -> Just val
            Nothing -> Just $ Symbol a
    replace' val = Just val


-- | Make an alist of items to replace from a `Rule` and usage
--
-- @(=> a b c ...) -> (=> 1 2 3 4 5) -> [(a, 1), (b, 2), (c, (3 4 5))]@
rewrites :: [Identifier] -> Rule -> Scheme -> [(String, Scheme)]
rewrites ids (Rule (List pattn') _template) (List usage) =
    reverse $ foldl dedup [] $ zip pattn usage
  where
    Symbol name = head pattn

    -- Pattern might be shorter than usage, so we append `...`
    pattn = pattn' ++ repeat (Symbol "...")

    -- Squash `alist` with duplicates and `...`
    -- >> dedup [("a",1), ("b",2), ("c",3), ("...",4), ("...",5)]
    -- [("a",1), ("b",2), ("c", '(3, 4, 5)]

    dedup :: [(String, Scheme)] -> (Pattern, Scheme) -> [(String, Scheme)]
    dedup [] (Ellipses, _var) = error "Unexpected `...`, expected binding"
    dedup ((binding, List xs): st) (Ellipses, s) =
        (binding, List (reverse $ s: reverse xs)): st
    dedup ((binding, old): st) (Ellipses, s) = (binding, List [old, s]): st
    dedup st (Symbol a, s)
      -- Ignore macro name
      | a == name = st
      -- Ignore identifiers in rewrites
      | Symbol a `elem` ids = st
      | otherwise = (a, s): st
    dedup _ _ = error $ "Unknown exp in dedup: " ++ show usage


rewrites _ids pattn usage =
    error $ concat ["Malformed pattern ", show pattn, " for usage: ", show usage]

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
match ids pattn usage =
    case (literalp, pattn, usage) of

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
    literalp = pattn `elem` ids


-- § Exposed API

-- | Compile the AST and return the result, along with recognized macros
--
-- This method is stateless and subsequent applications wont behave like a REPL.
compile :: [Scheme] -> Either String ([Scheme], Macros)
compile ast = runIdentity $ runExceptT $ runStateT compiled macros
  where
    compiled = catMaybes <$> mapM compile1 ast
    macros = []
