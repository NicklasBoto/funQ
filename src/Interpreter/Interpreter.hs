{-# language LambdaCase #-}

module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
    ( MonadTrans(lift), ExceptT, MonadError(throwError) )
import Control.Monad.Reader ()
import Data.List ()
import Control.Monad.Identity ()
import qualified FunQ as Q
import Control.Monad.State ()
import Data.Functor ( (<&>) )
import Parser.Abs as Abs
    ( Gate(GS, GH, GX, GY, GZ, GI, GT, GCNOT, GTOF, GSWP, GFRDK, GQFT, GQFTI, 
      GCR2,GCR2D,GCR4,GCR4D,GCR8,GCR8D),
      Bit(BOne, BZero)  )
import qualified AST.AST as A

-- TODO:
-- fredkin/toffoli

-- När vi tycker interpreter är klar! 
-- main driver: egen fil? turtle och haskelline? 

-- Nice to Have:
-- evaluera uttryck utan fil.
-- let user define custom gates (needs syntax for gate definition, type checking of arbitrary gate and evaluation of it)
-- flera let defs irad (istälelt för behöva flera lets) ()
-- explicita tuples i funktionsargumentet, á la f (x,y) = cnot (x,y) (typ pattern matching)

-- Tidigare förslag:
-- typeclass runnable, ta in gate och a, spotta ut QM a
-- kan definiera olika fel



data ValueError
    = NotFunction String
    | NoMainFunction String
    | NotApplied String
    | Fail String
     deriving Show

type Sig = M.Map String A.Term
type Eval a = ExceptT ValueError Q.QM a

-- | Environment type, stores bound variables & functions
data Env = Env {
      values    :: [Value]
    , functions :: Sig
} deriving Show

instance Show Value where
    show (VBit b)    = show b
    show (VTup a b)   = "(" ++ show a ++ "," ++ show b ++ ")"
    show (VQBit q)   = show q
    show VUnit       = "*"
    show (VFunc _ t) = "Function " ++ show t

-- | Main function in interpreter (exported)
interpret :: [A.Function] -> Eval Value
interpret fs = do
    let env = createEnv fs
    eval env =<< getMainTerm env

-- | Creates an environment from a list of functions. 
createEnv :: [A.Function] -> Env
createEnv fs = Env { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                     values = []}

-- | Fetches program main function
getMainTerm :: Env -> Eval A.Term
getMainTerm env = case M.lookup "main" (functions env) of
    Just term -> return term
    Nothing   -> throwError $ NoMainFunction "Main function not defined"

-- | Return type
data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup Value Value
    | VFunc [Value] A.Term

-- | Term evaluator
eval :: Env -> A.Term -> Eval Value
eval env = \case
    A.Idx j -> return $ values env !! fromIntegral j

    A.Fun s -> case M.lookup s (functions env) of
        Just t  -> eval env t
        Nothing -> throwError $ NotFunction $ "Function " ++ show s ++ " is not defined"

    A.Bit BZero -> return $ VBit 0
    A.Bit BOne -> return $ VBit 1

    A.Tup t1 t2 -> do
        v1 <- eval env t1
        v2 <- eval env t2
        return $ VTup v1 v2

    A.App e1 e2 -> case e1 of
        A.Gate g -> case g of
            Abs.GH    -> runGate  Q.hadamard e2 env
            Abs.GX    -> runGate  Q.pauliX e2 env
            Abs.GY    -> runGate  Q.pauliY e2 env
            Abs.GZ    -> runGate  Q.pauliZ e2 env
            Abs.GI    -> runGate  Q.identity e2 env
            Abs.GT    -> runGate  Q.phasePi8 e2 env
            Abs.GS    -> runGate  Q.phase e2 env
            Abs.GCNOT -> run2Gate Q.cnot e2 env
            Abs.GTOF  -> run3Gate Q.toffoli e2 env
            Abs.GSWP  -> run2Gate Q.swap e2 env
            Abs.GFRDK -> run3Gate Q.fredkin e2 env
            Abs.GQFT  -> runQFT   Q.qft e2 env
            Abs.GQFTI -> runQFT   Q.qftDagger e2 env
            Abs.GCR2  -> run2Gate (`Q.cphase` (pi/2)) e2 env
            Abs.GCR2D  -> run2Gate (`Q.cphase` (-pi/2)) e2 env
            Abs.GCR4  -> run2Gate (`Q.cphase` (pi/4)) e2 env
            Abs.GCR4D  -> run2Gate (`Q.cphase` (-pi/4)) e2 env
            Abs.GCR8  -> run2Gate (`Q.cphase` (-pi/8)) e2 env
            Abs.GCR8D  -> run2Gate (`Q.cphase` (-pi/8)) e2 env

        A.New -> do
            VBit b' <- eval env e2
            lift $ Q.new b' <&> VQBit
        A.Meas -> do
            VQBit q' <- eval env e2
            lift $ Q.measure q' <&> VBit
        _ -> do
            v2 <- eval env e2
            VFunc v1 a <- eval env e1
            eval env{ values = v2 : v1 ++ values env } a

    A.IfEl bit l r -> do
        VBit b <- eval env bit
        eval env $ if b == 1 then l else r

    A.Let eq inn -> do
         VTup x1 x2 <- eval env eq
         eval env{ values = x1 : x2 : values env } inn

    A.Abs e  -> return $ VFunc (values env) e
    A.Unit   -> return VUnit
    A.Gate g -> return $ VFunc (values env) (A.App (A.Gate g) (A.Idx 0))
    A.New    -> return $ VFunc (values env) (A.App A.New (A.Idx 0))
    A.Meas   -> return $ VFunc (values env) (A.App A.Meas (A.Idx 0))

fromVTup :: Value -> [Value]
fromVTup (VTup a b) = a : fromVTup b
fromVTup         x  = [x]

toVTup :: [Value] -> Value
toVTup = foldr1 VTup

-- | Eval monad print
printE :: Show a => a -> Eval ()
printE = lift . Q.io . print

-- | Run QFT gate
runQFT :: ([Q.QBit] -> Q.QM [Q.QBit]) -> A.Term -> Env -> Eval Value
runQFT g q env = do
    res <- eval env q
    case res of
        (VQBit q') ->
            lift (g [q']) <&> VQBit . head
        vt@(VTup _ _) -> do
            b <- lift $ g (unValue (fromVTup vt))
            return $ toVTup $ map VQBit b
        where unValue []            = []
              unValue (VQBit q:qss) = q : unValue qss

-- | Run gate taking one qubit
runGate :: (Q.QBit -> Q.QM Q.QBit) -> A.Term -> Env -> Eval Value
runGate g q env = do
    VQBit q' <- eval env q
    VQBit <$> lift (g q')

-- | Run gate taking two qubits
run2Gate :: ((Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit)) -> A.Term -> Env -> Eval Value
run2Gate g q env = do
    VTup (VQBit a) (VQBit b) <- eval env q
    (p,q) <- lift (g (a,b))
    return $ VTup (VQBit p) (VQBit q)

-- | Run gate taking three qubits
run3Gate :: ((Q.QBit, Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit, Q.QBit)) -> A.Term -> Env -> Eval Value
run3Gate g q env = do
    [VQBit a, VQBit b, VQBit c] <- fromVTup <$> eval env q
    toVTup . tupToList <$> lift (g (a,b,c))
        where tupToList (a,b,c) = [VQBit a, VQBit b, VQBit c]