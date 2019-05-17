{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.Diamondback.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import           Data.Monoid
import qualified Data.List          as L
import           Language.Diamondback.Types
import           Language.Diamondback.Utils

--------------------------------------------------------------------------------
check :: BareProgram -> BareProgram
--------------------------------------------------------------------------------
check p = case wellFormed p of
            [] -> p
            es -> throw es

-- | Map from function name to arity
type FunEnv = Env

--------------------------------------------------------------------------------
-- | `wellFormed p` returns the list of errors for a program `p`
--------------------------------------------------------------------------------
wellFormed :: BareProgram -> [UserError]
--------------------------------------------------------------------------------
wellFormed (Prog ds e) = duplicateFunErrors ds
                      ++ concatMap (wellFormedD fEnv) ds
                      ++ wellFormedE fEnv emptyEnv e
  where
    fEnv               = fromListEnv [(bindId f, length xs) | Decl f xs _ _ <- ds]

--------------------------------------------------------------------------------
-- | `wellFormedD fEnv vEnv d` returns the list of errors for a func-decl `d`
--------------------------------------------------------------------------------
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = wellFormedE fEnv vEnv e  ++ duplicate
  where
    vEnv = addsEnv emptyEnv xs
    duplicate = duplicateParamErrs dupParams
    dupParams = repeated xs ids 
    ids       = idify xs


addsEnv :: Env -> [Bind a] -> Env
addsEnv env []     = env
addsEnv env (x:xs) = addsEnv (addEnv x env) xs


idify :: [Bind a] -> [Id]
idify [] = []
idify (x:xs) = [bindId x] ++ idify xs
                   
duplicateParamErrs :: (Located (Bind a)) => [Bind a] -> [UserError]
duplicateParamErrs [] = []
duplicateParamErrs (x:xs) = ((errDupParam x):duplicateParamErrs xs)

 
repeated :: [Bind a] -> [Id] -> [Bind a]
repeated binds ids = nub' binds ids []
  where
    nub' [] [] repeats = repeats
    nub' (x:xs) (y:ys) ls
      | y `elem` ys   = nub' xs ys (x:ls) -- if id y is repeated, add its bind to repeated list
      | otherwise     = nub' xs ys ls -- if y was not repeated, recurse on the list

--------------------------------------------------------------------------------
-- | `wellFormedE vEnv e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv env e = go env e
  where
    gos vEnv es               = concatMap (go vEnv) es
    go _    (Boolean {})      = []
    go _    (Number  n     l) = overflow
      where
        overflow = if (n > maxInt) then
                    [errLargeNum l n]
                   else
                     []
    go vEnv (Id      x     l) = case (lookupEnv x vEnv) of
                                  Just num -> []
                                  Nothing  -> [errUnboundVar l x]
    go vEnv (Prim1 _ e     _) = go  vEnv e
    go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]
    go vEnv (Let x e1 e2   _) = go vEnv e1
                             ++ shadowError
                             ++ go (addEnv x vEnv) e2
      where
        shadowError          = case (lookupEnv (bindId x) vEnv) of
                                  Nothing -> []
                                  Just i  -> [errDupBind x] -- this let is redefining a variable already in scope
    go vEnv (App f es      l) = unboundFunErrors fEnv f l es
                             ++ gos vEnv es

--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors
  = fmap errDupFun
  . concat
  . dupBy (bindId . fName)

-- fEnv, a map from function-names to the function-arity (number of params)
-- this function handles arity and undefined function errors
unboundFunErrors :: FunEnv -> Id -> SourceSpan -> [Expr a] -> [UserError]
unboundFunErrors fEnv f l es = errors
  where
    args = length es -- number of parameters passed into function
    errors = case (lookupEnv f fEnv) of -- expected number of params for this func 
                Just arity -> if (arity == args) then [] 
                              else [errCallArity l f] -- arity error
                Nothing  -> [errUnboundFun l f] -- f wasn't found in fEnv - undefined func!

-- | `maxInt` is the largest number you can represent with 31 bits (accounting for sign
--    and the tag bit.

maxInt :: Integer
maxInt = 1073741823

--------------------------------------------------------------------------------
-- | Error Constructors: Use these functions to construct `UserError` values
--   when the corresponding situation arises. e.g see how `errDupFun` is used.
--------------------------------------------------------------------------------

errDupFun :: (Located (Bind a)) => Decl a -> UserError
errDupFun d = mkError "duplicate function" (sourceSpan f) where f = fName d

errDupParam :: (Located (Bind a)) => Bind a -> UserError
errDupParam x = mkError "duplicate parameter" (sourceSpan x)

errDupBind :: (Located (Bind a)) => Bind a -> UserError
errDupBind x = mkError "shadow binding" (sourceSpan x)

errLargeNum :: SourceSpan -> Integer -> UserError
errLargeNum   l _n = mkError "too large" l

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l _x = mkError "unbound variable" l

errUnboundFun :: SourceSpan -> Id -> UserError
errUnboundFun l _f = mkError "not defined" l

errCallArity :: SourceSpan -> Id -> UserError
errCallArity  l _f = mkError "arity" l
