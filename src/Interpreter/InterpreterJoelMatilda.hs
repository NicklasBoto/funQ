{-# LANGUAGE LambdaCase #-}

-- | Interpreter for lambda-calculus with if, +, -, <.
--
--   Strategy can be either call-by-value or call-by-name.

module Interpreter where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

import Fun.Abs
import Fun.Print

type Err = Except String

-- | Evaluation strategy.

data Strategy
  = CallByName
  | CallByValue

-- | Entry point: Program computes a number.
-- [Function] -> Err Value? (Bit [Bit]?)
interpret :: Strategy -> Program -> Err Integer
interpret strategy (Prog defs (DMain mainExp)) = do
  let mkLam xs e = foldr EAbs e xs
  let mkDef (DDef f xs e) = (f, mkLam xs e)
  let sig = Map.fromList $ map mkDef defs
  let cxt = Cxt
        { cxtStrategy = strategy
        , functions   = sig
        , variables   = Map.empty
        }
  -- Run the interpreter.
  v <- eval cxt mainExp
  -- Return the result.
  intValue v

---------------------------------------------------------------------------
-- * Data structures for the interpreter.
---------------------------------------------------------------------------

-- | Context.

data Cxt = Cxt
  { cxtStrategy :: Strategy  -- ^ Evaluation strategy (fixed).
  , functions   :: Sig       -- ^ Binds function identifiers to expression.
  , variables   :: Env       -- ^ Binds local variables to values.
  }

-- | Values.
data Value
  = VInt Integer             -- ^ Integer literal.
  | VClos Ident Exp Env      -- ^ Function: lambda-closure ⟨λx→e;γ⟩
  deriving (Show)
                             --   FV(e) ⊆ {x} ∪ dom(γ)

-- | Signature.
type Sig = Map Ident Exp

-- | Environment.
type Env = Map Ident Entry
type Entry = Value

intValue :: Value -> Err Integer
intValue = \case
  VInt i  -> return i
  VClos{} -> throwError $ "Integer value expected, but got function value"

apply :: Cxt -> Value -> Value -> Err Value
apply cxt f v =
  case f of
    VInt{} -> throwError $ "Integer value cannot be applied, expected function value"
    VClos x e env -> eval cxt{ variables = Map.insert x v env } e

---------------------------------------------------------------------------
-- * Interpreter.
---------------------------------------------------------------------------

-- | Evaluation.
eval :: Cxt -> Exp -> Err Value
eval cxt = \case

  EInt i    -> return $ VInt i

  EVar x    -> do
    case Map.lookup x $ variables cxt of
      Just v  -> case cxtStrategy cxt of 
        -- CBV simply returns value since it has already been evaluated
        CallByValue -> return v 
        -- CBN simply evaluates the closure
        CallByName -> case v of 
            VClos _ ex env -> eval cxt { variables = env } ex 
            _              -> throwError "Call-by-name wrongly implemented, must be a closure"
      Nothing -> case Map.lookup x $ functions cxt of
        -- Evaluate function without interferring with its state
        Just e  -> eval cxt{ variables = Map.empty } e
        -- No variable/function present, i.e. an unbound variable
        Nothing -> do
          throwError $ "unbound variable " ++ printTree x

  EAbs x e  -> return $ VClos x e (variables cxt)

  EApp f a -> do 
     g <- eval cxt f
     case cxtStrategy cxt of 
        CallByValue -> do
           v <- eval cxt a
           apply cxt g v
        CallByName -> case g of 
          VInt i -> throwError $ "Application must be done using a function, not integer " ++ show i
          VClos x _ env -> do
            -- Create Closure given the current environment (same as if evaluated by CBV)
            let u = VClos x a (variables cxt)
            -- Bind the closure to the variable x, effectively also performing overshadowing
            apply (cxt {variables = Map.insert x u env}) g u 
        
  EAdd e e' -> applyIntBinOp cxt e e' (+)

  ESub e e' -> applyIntBinOp cxt e e' (-)

  ELt  e e' -> applyIntBinOp cxt e e' (\a b -> if (a < b) then 1 else 0)

  EIf c t e -> do
    i <- eval cxt c >>= intValue
    if i == 1 then eval cxt t else eval cxt e
 
applyIntBinOp :: Cxt -> Exp -> Exp -> (Integer -> Integer -> Integer) -> Err Value  
applyIntBinOp cxt e1 e2 op = do 
    e1' <- eval cxt e1  >>= intValue
    e2' <- eval cxt e2  >>= intValue
    return $ VInt (e1' `op` e2')