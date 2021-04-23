{-# language LambdaCase #-}

module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
    ( MonadTrans(lift), ExceptT, MonadError(throwError) )
import qualified FunQ as Q
import Lib.QM (link)
import Parser.Abs as Abs
import qualified AST.AST as A
import Parser.Print

data ValueError
    = NotFunction String
    | NotApplied String
    | Fail String
    | IndexTooLarge String
     deriving Show

type Sig = M.Map String A.Term
type Eval = ExceptT ValueError Q.QM

-- | Environment type, stores bound variables & functions
data Env = Env {
      values    :: [Value]
    , functions :: Sig
} deriving Show

instance Show Value where
    show (VBit b)      = show b
    show (VTup a b)    = "⟨" ++ show a ++ "," ++ show b ++ "⟩"
    show (VQBit q)     = "p" ++ show (link q)
    show VUnit         = "*"
    show (VAbs _ t e) = show (A.Abs t e)
    show VNew          = "new"
    show VMeas         = "measure"
    show (VGate g)     = printTree g



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
    Nothing   -> throwError $ Fail "Main function not defined" 

-- | Return type
data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup Value Value
    | VAbs [Value] A.Type A.Term
    | VNew 
    | VMeas
    | VGate Gate

-- | Term evaluator
eval :: Env -> A.Term -> Eval Value
eval env = \case
    A.Idx j -> if j >= fromIntegral (length (values env)) then throwError (IndexTooLarge ("Index" ++ show j ++ "is too large"))
             else return $ values env !! fromIntegral j

    A.Fun s -> case M.lookup s (functions env) of
        Just t  -> eval env t
        Nothing -> throwError $ NotFunction $ "Function " ++ show s ++ " is not defined"

    A.Bit A.BZero -> return $ VBit 0
    A.Bit A.BOne -> return $ VBit 1

    A.Tup t1 t2 -> do
        v1 <- eval env t1
        v2 <- eval env t2
        return $ VTup v1 v2

    A.App e1 e2 -> case e1 of
        A.Gate g -> case g of
            Abs.GH     -> runGate  Q.hadamard e2 env
            Abs.GX     -> runGate  Q.pauliX e2 env
            Abs.GY     -> runGate  Q.pauliY e2 env
            Abs.GZ     -> runGate  Q.pauliZ e2 env
            Abs.GI     -> runGate  Q.identity e2 env
            Abs.GT     -> runGate  Q.phasePi8 e2 env
            Abs.GS     -> runGate  Q.phase e2 env
            Abs.GCNOT  -> run2Gate Q.cnot e2 env
            Abs.GTOF   -> run3Gate Q.toffoli e2 env
            Abs.GSWP   -> run2Gate Q.swap e2 env
            Abs.GFRDK  -> run3Gate Q.fredkin e2 env
            Abs.GQFT   -> runQFT   (Q.qft 1) e2 env
            Abs.GQFTI  -> runQFT   (Q.qftDagger 1) e2 env
            Abs.GQFT2  -> runQFT   (Q.qft 2) e2 env
            Abs.GQFTI2 -> runQFT   (Q.qftDagger 2) e2 env
            Abs.GQFT3  -> runQFT   (Q.qft 3) e2 env
            Abs.GQFTI3 -> runQFT   (Q.qftDagger 3) e2 env
            Abs.GQFT4  -> runQFT   (Q.qft 4) e2 env
            Abs.GQFTI4 -> runQFT   (Q.qftDagger 4) e2 env
            Abs.GQFT5  -> runQFT   (Q.qft 5) e2 env
            Abs.GQFTI5 -> runQFT   (Q.qftDagger 5) e2 env
            Abs.GCR    -> run2Gate (`Q.cphase` ( 1/2)) e2 env
            Abs.GCRD   -> run2Gate (`Q.cphase` (-1/2)) e2 env
            Abs.GCR2   -> run2Gate (`Q.cphase` ( 1/4)) e2 env
            Abs.GCR2D  -> run2Gate (`Q.cphase` (-1/4)) e2 env
            Abs.GCR3   -> run2Gate (`Q.cphase` ( 1/3)) e2 env
            Abs.GCR3D  -> run2Gate (`Q.cphase` (-1/3)) e2 env
            Abs.GCR4   -> run2Gate (`Q.cphase` ( 1/8)) e2 env
            Abs.GCR4D  -> run2Gate (`Q.cphase` (-1/8)) e2 env
            Abs.GCR8   -> run2Gate (`Q.cphase` ( 1/16)) e2 env
            Abs.GCR8D  -> run2Gate (`Q.cphase` (-1/16)) e2 env

        A.New -> do
            VBit b' <- eval env e2
            lift $ VQBit <$> Q.new b'
        A.Meas -> do
            VQBit q' <- eval env e2
            lift $ VBit <$> Q.measure q'
        _ -> do
            v2 <- eval env e2
            v1 <- eval env e1
            case v1 of
                VAbs v1 _ a -> eval env{ values = v2 : v1 ++ values env } a
                VNew -> eval env{ values = v2 : values env } (A.App A.New (A.Idx 0))
                VMeas -> eval env{ values = v2 : values env } (A.App A.Meas (A.Idx 0))
                (VGate g) -> eval env{ values = v2 : values env } (A.App (A.Gate g) (A.Idx 0))
                _ -> throwError $ Fail $ "Can't apply " ++ show v1 ++ " with " ++ show v2

    A.IfEl bit l r -> do
        VBit b <- eval env bit
        eval env $ if b == 1 then l else r

    A.Let eq inn -> do
         VTup x1 x2 <- eval env eq
         eval env{ values = x2 : x1 : values env } inn

    A.Abs t e  -> return $ VAbs (values env) t e
    A.Unit   -> return VUnit
    A.Gate g -> return $ VGate g 
    A.New    -> return VNew
    A.Meas   -> return VMeas

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
            VQBit . head <$> lift (g [q'])
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