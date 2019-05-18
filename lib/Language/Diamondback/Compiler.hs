{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

--------------------------------------------------------------------------------
-- | The entry point for the compiler: a function that takes a Text
--   representation of the source and returns a (Text) representation
--   of the assembly-program string representing the compiled version
--------------------------------------------------------------------------------

module Language.Diamondback.Compiler ( compiler, compile ) where

import           Data.Monoid
import           Control.Arrow                    ((>>>))
import           Prelude                  hiding (compare)
import           Control.Monad                   (void)
import           Data.Maybe
import           Data.Bits                       (shift)
import           Language.Diamondback.Types      hiding (Tag)
import           Language.Diamondback.Parser     (parse)
import           Language.Diamondback.Checker    (check, errUnboundVar)
import           Language.Diamondback.Normalizer (anormal)
import           Language.Diamondback.Label
import           Language.Diamondback.Asm        (asm)


--------------------------------------------------------------------------------
compiler :: FilePath -> Text -> Text
--------------------------------------------------------------------------------
compiler f = parse f >>> check >>> anormal >>> tag >>> tails >>> compile >>> asm


--------------------------------------------------------------------------------
-- | The compilation (code generation) works with AST nodes labeled by @Ann@
--------------------------------------------------------------------------------
type Ann   = ((SourceSpan, Int), Bool)
type AExp  = AnfExpr Ann
type IExp  = ImmExpr Ann
type ABind = Bind    Ann
type ADcl  = Decl    Ann
type APgm  = Program Ann

instance Located Ann where
  sourceSpan = fst . fst

instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

annTag :: Ann -> Int
annTag = snd . fst

annTail :: Ann -> Bool
annTail = snd


--------------------------------------------------------------------------------
compile :: APgm -> [Instruction]
--------------------------------------------------------------------------------
compile (Prog ds e) = compileBody emptyEnv e
                   ++ concatMap compileDecl ds

compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e l) = ILabel (DefFun (bindId f))
                            : compileBody (paramsEnv xs) e

paramsEnv :: [Bind a] -> Env
paramsEnv xs = fromListEnv (zip xids [-2, -3..])
  where
      xids     = map bindId xs


compileBody :: Env -> AExp -> [Instruction]
compileBody env e = funInstrs (countVars e) (compileEnv env e)

-- | @funInstrs n body@ returns the instructions of `body` wrapped
--   with code that sets up the stack (by allocating space for n local vars)
--   and restores the callees stack prior to return.

funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs
  = funEntry n
 ++ instrs
 ++ funExit
 ++ [IRet]

-- FILL: insert instructions for setting up stack for `n` local vars
funEntry :: Int -> [Instruction]
funEntry e = [ IPush (Reg EBP)
             , IMov  (Reg EBP) (Reg ESP)
             , ISub  (Reg ESP) (Const tmp)
             ]
  where
    tmp     = 4 * e 

-- FILL: clean up stack & labels for jumping to error
funExit :: [Instruction]
funExit = [ IMov (Reg ESP) (Reg EBP)
          , IPop (Reg EBP)
          ]

--------------------------------------------------------------------------------
-- | @countVars e@ returns the maximum stack-size needed to evaluate e,
--   which is the maximum number of let-binds in scope at any point in e.
--------------------------------------------------------------------------------
countVars :: AnfExpr a -> Int
--------------------------------------------------------------------------------
countVars (Let _ e b _)  = max (countVars e) (1 + countVars b)
countVars (If v e1 e2 _) = maximum [countVars v, countVars e1, countVars e2]
countVars _              = 0

--------------------------------------------------------------------------------
compileEnv :: Env -> AExp -> [Instruction]
--------------------------------------------------------------------------------
compileEnv env v@Number {}       = [ compileImm env v  ]

compileEnv env v@Boolean {}      = [ compileImm env v  ]

compileEnv env v@Id {}           = [ compileImm env v  ]

compileEnv env e@Let {}          = is ++ compileEnv env' body
  where
    (env', is)                   = compileBinds env [] binds
    (binds, body)                = exprBinds e

compileEnv env (Prim1 o v l)     = compilePrim1 l env o v

compileEnv env (Prim2 o v1 v2 l) = compilePrim2 l env o v1 v2

compileEnv env (If v e1 e2 l)    = compileIf l env v e1 e2

compileEnv env (App f vs _)     = call (DefFun f) [param env v | v <- vs]


-- call function
call :: Label -> [Arg] -> [Instruction]
call (DefFun fName) args     = putOnStack args' []
  where
    putOnStack [] instrs     = instrs ++ funcCall 
    putOnStack (a:as) instrs = putOnStack as ([IPush a] ++ instrs) 
    argNum                   = length args
    funcCall                 = [ICall (DefFun fName),
                                IAdd (Reg ESP) (Const disp)]
    disp                     = argNum * 4
    args'                    = reverse args

-- compileImm
compileImm :: Env -> IExp -> Instruction
compileImm env v = IMov (Reg EAX) (immArg env v)

compileBinds :: Env -> [Instruction] -> [(ABind, AExp)] -> (Env, [Instruction])
compileBinds env is []     = (env, is)
compileBinds env is (b:bs) = compileBinds env' (is <> is') bs
  where
    (env', is')            = compileBind env b

compileBind :: Env -> (ABind, AExp) -> (Env, [Instruction])
compileBind env (x, e) = (env', is)
  where
    is                 = compileEnv env e
                      <> [IMov (stackVar i) (Reg EAX)]
    (i, env')          = pushEnv x env

immArg :: Env -> IExp -> Arg
immArg _   (Number n _)  = repr n
immArg _   (Boolean b _) = repr b
immArg env e@(Id x _)    = stackVar (fromMaybe err (lookupEnv x env))
  where
    err                  = abort (errUnboundVar (sourceSpan e) x)
immArg _   e             = panic msg (sourceSpan e)
  where
    msg                  = "Unexpected non-immExpr in immArg: " <> show (void e)

param :: Env -> IExp -> Arg
param env v = Sized DWordPtr (immArg env v)

stackVar :: Int -> Arg
stackVar i = RegOffset (-4 * i) EBP

compilePrim1 :: Ann -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env op v = compileEnv env v -- this should put the value in EAX 
                       ++ instrs
   where
    instrs              = case op of
                            Add1   -> errorChecking ++ [IAdd (Reg EAX) (Const 2),
                                                        IJo (DynamicErr (ArithOverflow))]
                            Sub1   -> errorChecking ++ [ISub (Reg EAX) (Const 2),
                                                        IJo (DynamicErr (ArithOverflow))]
                            Print  -> [IPush (Reg EAX),
                                       ICall (Builtin ("print"))]
                            IsNum  -> genCheck ++
                                      [IJe (BranchTrue tag),
                                       IMov (Reg EAX) (HexConst 0x7fffffff),
                                       IJmp (BranchDone tag),
                                       ILabel (BranchTrue tag),
                                       IMov (Reg EAX) (HexConst 0xffffffff),
                                       ILabel (BranchDone tag)]
                            IsBool -> genCheck ++ 
                                      [IJne (BranchTrue tag),
                                       IMov (Reg EAX) (HexConst 0x7fffffff),
                                       IJmp (BranchDone tag),
                                       ILabel (BranchTrue tag),
                                       IMov (Reg EAX) (HexConst 0xffffffff),
                                       ILabel (BranchDone tag)]
                                      --[IJne (Builtin ("printNotBool"))]
    errorChecking       = genCheck ++
                          [IJne (DynamicErr (TypeError TNumber))]
    genCheck            = [IMov (Reg EBX) (Reg EAX),
                           IAnd (Reg EBX) (HexConst (0x00000001)),
                           ICmp (Reg EBX) (Const 0)]
    tag                 = annTag l
                     

-- | TBD: Implement code for `Prim2` with appropriate type checking
compilePrim2 :: Ann -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus v1 v2    = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [IAdd (Reg EAX) (immArg env v2),
                                   IJo (DynamicErr (ArithOverflow))]
compilePrim2 l env Minus v1 v2   = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [ISub (Reg EAX) (immArg env v2),
                                   IJo (DynamicErr (ArithOverflow))]
compilePrim2 l env Times v1 v2   = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [IMul (Reg EAX) (immArg env v2),
                                   IJo (DynamicErr (ArithOverflow)),
                                   ISar (Reg EAX) (Const 1)]
compilePrim2 l env Less v1 v2    = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [ICmp (Reg EAX) (immArg env v2),
                                   IJl (BranchTrue tag1), -- if v1 < v2, put true
                                   IMov (Reg EAX) (HexConst 0x7fffffff),
                                   IJmp (BranchDone tag1),
                                   ILabel (BranchTrue tag1),
                                   IMov (Reg EAX) (HexConst 0xffffffff),
                                   ILabel (BranchDone tag1)] 
  where
    tag1 = annTag l
compilePrim2 l env Greater v1 v2 = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [ICmp (Reg EAX) (immArg env v2),
                                   IJg (BranchTrue tag1), -- if v1 > v2, put true
                                   IMov (Reg EAX) (HexConst 0x7fffffff),
                                   IJmp (BranchDone tag1),
                                   ILabel (BranchTrue tag1),
                                   IMov (Reg EAX) (HexConst 0xffffffff),
                                   ILabel (BranchDone tag1)] 
  where
    tag1 = annTag l
compilePrim2 l env Equal v1 v2   = assertNum env v1 ++
                                   assertNum env v2 ++
                                   compileEnv env v1 ++
                                   [ICmp (Reg EAX) (immArg env v2),
                                   IJe (BranchTrue tag1), -- if v1 == v2, put true
                                   IMov (Reg EAX) (HexConst 0x7fffffff),
                                   IJmp (BranchDone tag1),
                                   ILabel (BranchTrue tag1),
                                   IMov (Reg EAX) (HexConst 0xffffffff),
                                   ILabel (BranchDone tag1)] 
  where
    tag1          = annTag l

assertNum :: Env -> IExp -> [Instruction]
assertNum env v = [ IMov (Reg EAX) (immArg env v)
                  , IMov (Reg EBX) (Reg EAX)
                  , IAnd (Reg EBX) (HexConst 0x00000001)
                  , ICmp (Reg EBX) (Const 0)
                  , IJne (DynamicErr (TypeError TNumber))
                  ]

-- assertBool assumes you already evaluated the expression
-- and put it into Reg EAX
assertBool :: Env -> IExp -> [Instruction]
assertBool env v = [ IMov (Reg EBX) (Reg EAX)
                   , IAnd (Reg EBX) (HexConst 0x00000001)
                   , ICmp (Reg EBX) (Const 0)
                   , IJe (DynamicErr (TypeError TBoolean))
                   ]

-- | TBD: Implement code for `If` with appropriate type checking
compileIf :: Ann -> Env -> IExp -> AExp -> AExp -> [Instruction]
compileIf l env v e1 e2 = compileEnv env v ++  -- move it to EAX
                          assertBool env v ++  -- compare it to a boolean
                          [ICmp (Reg EAX) (HexConst 0xffffffff),
                          IJe (BranchTrue tag1)] ++ -- do the if body if it's true
                          compileEnv env e2 ++   -- do the else statement here
                          [IJmp (BranchDone tag1),
                          ILabel (BranchTrue tag1)] ++
                          compileEnv env e1 ++
                          [ILabel (BranchDone tag1)]
  where
    tag1 = annTag l


--------------------------------------------------------------------------------
-- | Representing Values
--------------------------------------------------------------------------------

class Repr a where
  repr :: a -> Arg

instance Repr Bool where
  repr True  = HexConst 0xffffffff
  repr False = HexConst 0x7fffffff

instance Repr Int where
  repr n = Const (fromIntegral (shift n 1))

instance Repr Integer where
  repr n = Const (fromIntegral (shift n 1))

typeTag :: Ty -> Arg
typeTag TNumber   = HexConst 0x00000000
typeTag TBoolean  = HexConst 0x7fffffff

typeMask :: Ty -> Arg
typeMask TNumber  = HexConst 0x00000001
typeMask TBoolean = HexConst 0x7fffffff
