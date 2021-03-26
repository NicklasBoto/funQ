{-# language LambdaCase #-}

module Interpreter.Interpreter where

import System.IO ()

import qualified Data.Map as M
import Control.Monad.Except
    ( MonadTrans(lift), ExceptT, MonadError(throwError), runExceptT )
import Control.Monad.Reader ()
import Data.List ()
import Control.Monad.Identity ()
import qualified FunQ as Q
import Control.Monad.State ()
import Parser.Abs as Abs
    ( Gate(GS, GH, GX, GY, GZ, GI, GT, GCNOT, GTOF, GSWP, GFRDK),
      Bit(BOne, BZero),
      Program )
import qualified AST.AST as A

-- TODO:
-- test suite
-- generell run gate (för att få med quantum fourier, genom Runnable TypeClass ex)
-- teleportera 1, och få 1

-- När vi tycker interpreter är klar! 
-- main driver: egen fil? turtle och haskelline? 

-- Nice to Have:
-- let user define custom gates (needs syntax for gate definition, type checking of arbitrary gate and evaluation of it)


data Error
    = NotFunction String
    | NoMainFunction String
    | NotApplied String
    | Fail String
    | IndexTooLarge String
     deriving Show

type Sig = M.Map String A.Term

type Eval a = ExceptT Error Q.QM a

data Ctx = Ctx {
      values    :: [Value]
    , functions :: Sig 
}

instance Show Value where
    show (VBit b)    = show b
    show (VTup bs)   = show bs
    show (VQBit q)   = show q
    show VUnit       = "*"
    show (VFunc _ t) = "Function " ++ show t

-- Main function in interpreter (which we export)
interpret :: [A.Function] -> Eval Value
interpret fs = do
    -- Create context 
    let ctx = createctxs fs
    -- Eval main function
    mainTerm <- getMainTerm ctx
    -- Return the return value from main
    eval ctx mainTerm

-- | Creates a context from a list of functions. 
createctxs :: [A.Function] -> Ctx
createctxs fs = Ctx { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                            values = []}

getMainTerm :: Ctx -> Eval A.Term
getMainTerm ctx = case M.lookup "main" (functions ctx) of
    Just term -> return term
    Nothing   -> throwError $ NoMainFunction "Main function not defined"

data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup [Value]
    | VFunc [Value] A.Term -- could [Value] be removed?

eval :: Ctx -> A.Term -> Eval Value     
eval ctx = \case
    A.Idx j -> do
        if fromIntegral j >= length (values ctx) then do
            throwError $ IndexTooLarge $ "Index " ++ show j ++ " too large, Values=" ++ concat (map show (values ctx))
         else do 
             return $ values ctx !! fromInteger j

    A.Fun s -> case M.lookup s (functions ctx) of
        Just t  -> eval ctx t
        Nothing -> throwError $ NotFunction $ "Function " ++ show s ++ " is not defined" 

    A.Bit BZero -> return $ VBit 0
    A.Bit BOne -> return $ VBit 1

    A.Tup bs -> VTup <$> mapM (eval ctx) bs

    A.App (A.Gate g) q -> case g of
         Abs.GH    -> runGate  Q.hadamard q ctx
         Abs.GX    -> runGate  Q.pauliX q ctx
         Abs.GY    -> runGate  Q.pauliY q ctx
         Abs.GZ    -> runGate  Q.pauliZ q ctx
         Abs.GI    -> runGate  Q.identity q ctx
         Abs.GT    -> runGate  Q.tdagger q ctx
         Abs.GS    -> runGate  Q.phase q ctx
         Abs.GCNOT -> run2Gate Q.cnot q ctx
         Abs.GTOF  -> run3Gate Q.toffoli q ctx
         Abs.GSWP  -> run2Gate Q.swap q ctx
         Abs.GFRDK -> run3Gate Q.fredkin q ctx      
        -- GGate GateIdent
        --  , phasePi8
        --  , urot
        --  , crot
        --  , qft

        -- typeclass runnable, ta in gate och a, spotta ut QM a
        -- kan definiera olika fel
        
    A.App A.New b -> do
        VBit b' <- eval ctx b
        q <- lift $ Q.new b'
        return $ VQBit q

    A.App A.Meas q -> do
        VQBit q' <- eval ctx q
        b <- lift $ Q.measure q' 
        return $ VBit b

    A.App e1 e2 -> do 
        VFunc _ a <- eval ctx e1
        v <- eval ctx e2
        eval ctx{ values = v : values ctx } a
    
    A.IfEl bit l r -> do
        VBit b <- eval ctx bit 
        eval ctx $ if b == 1 then l else r
        
    A.Let eq inn -> do 
        VTup [x1, x2] <- eval ctx eq 
        eval ctx{ values = x2 : x1 : values ctx } inn

    A.Abs e  -> return $ VFunc (values ctx) e

    A.Void   -> return VUnit

    A.Gate g -> throwError $ NotApplied $ "Gate " ++ show g ++ " must be applied to something"

    A.New    -> throwError $ NotApplied "New must be applied to something"
    
    A.Meas   -> throwError $ NotApplied "Meas must be applied to something"


-- TODO: refactor more generally!
runGate :: (Q.QBit -> Q.QM Q.QBit) -> A.Term -> Ctx -> Eval Value
runGate g q ctx = do
    VQBit q' <- eval ctx q 
    VQBit <$> lift (g q')

run2Gate :: ((Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit)) -> A.Term -> Ctx -> Eval Value
run2Gate g q ctx = do
    VTup [VQBit a, VQBit b] <- eval ctx q
    res <- lift $ g (a,b)
    return $ VTup (tupToList res)
        where tupToList (a,b) = [VQBit a,VQBit b]

run3Gate :: ((Q.QBit, Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit, Q.QBit)) -> A.Term -> Ctx -> Eval Value
run3Gate g q ctx = do 
    VTup [VQBit a, VQBit b, VQBit c] <- eval ctx q
    res <- lift $ g (a,b,c)
    return $ VTup (tupToList res)
        where tupToList (a,b,c) = [VQBit a, VQBit b, VQBit c] 


