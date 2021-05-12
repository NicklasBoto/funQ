{-# language LambdaCase #-}
{-# language FlexibleInstances #-}

module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
    ( StateT, MonadState(get), evalStateT, modify )
import FunQ
import Lib.QM (link)
import qualified AST.AST as A

type Sig = M.Map String A.Term
type FunctionValues = M.Map String Value
type Eval = StateT FunctionValues QM

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
    show (VAbs _ t e)  = show (A.Abs t e)
    show VNew          = "new"
    show VMeas         = "measure"
    show (VGate g)     = show g

-- | Return type
data Value
    = VBit Bit
    | VQBit QBit
    | VUnit
    | VTup Value Value
    | VAbs [Value] A.Type A.Term
    | VNew
    | VMeas
    | VGate A.Gate

-- | Main function in interpreter (exported)
interpret :: [A.Function] -> QM Value
interpret fs = evalStateT main M.empty
    where env  = createEnv fs
          main = eval env (getTerm env "main")

-- | Creates an environment from a list of functions. 
createEnv :: [A.Function] -> Env
createEnv fs = Env { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                     values = []}

-- | Fetches a term from the environment
getTerm :: Env -> String -> A.Term
getTerm env name = let Just t = M.lookup name (functions env) in t

-- | Term evaluator
eval :: Env -> A.Term -> Eval Value
eval env (A.Bit A.BZero) = return $ VBit 0
eval env (A.Bit A.BOne)  = return $ VBit 1
eval env (A.Abs t e)     = return $ VAbs (values env) t e
eval env A.Unit          = return VUnit
eval env (A.Gate g)      = return $ VGate g
eval env A.New           = return VNew
eval env A.Meas          = return VMeas

eval env (A.Idx j) = return $ values env !! fromIntegral j

eval env (A.Fun s) = do
            let t = getTerm env s
            fs <- get
            case M.lookup s fs of
                (Just v) -> return v
                Nothing  -> do
                    v <- eval env t
                    modify (M.insert s v)
                    return v

eval env (A.Tup t1 t2) = do
    v1 <- eval env t1
    v2 <- eval env t2
    return $ VTup v1 v2

eval env (A.App t1 t2) =
    case t1 of
        A.Gate g -> case g of
            A.GH      -> runGate  hadamard t2 env
            A.GX      -> runGate  pauliX t2 env
            A.GY      -> runGate  pauliY t2 env
            A.GZ      -> runGate  pauliZ t2 env
            A.GI      -> runGate  identity t2 env
            A.GT      -> runGate  phasePi8 t2 env
            A.GS      -> runGate  phase t2 env
            A.GCNOT   -> run2Gate cnot t2 env
            A.GTOF    -> run3Gate toffoli t2 env
            A.GSWP    -> run2Gate swap t2 env
            A.GFRDK   -> run3Gate fredkin t2 env
            A.GQFT n  -> runQFT   (qft n) t2 env
            A.GQFTI n -> runQFT   (qftDagger n) t2 env
            A.GCR n   -> run2Gate (`cphase` (1/(n*2))) t2 env
            A.GCRI n  -> run2Gate (`cphase` (-1/(n*2))) t2 env
            A.GCCR n  -> run3Gate (`ccphase` (1/(n*2))) t2 env
            A.GCCRI n  -> run3Gate (`ccphase` (-1/(n*2))) t2 env

        A.New -> do
            VBit b' <- eval env t2
            lift $ VQBit <$> new b'
        A.Meas -> do
            VQBit q' <- eval env t2
            lift $ VBit <$> measure q'
        _ -> do
            v2 <- eval env t2
            v1 <- eval env t1
            case v1 of
                VAbs vs _ a -> eval env{ values = v2 : vs ++ values env } a
                VNew -> eval env{ values = v2 : values env } (A.App A.New (A.Idx 0))
                VMeas -> eval env{ values = v2 : values env } (A.App A.Meas (A.Idx 0))
                (VGate g) -> eval env{ values = v2 : values env } (A.App (A.Gate g) (A.Idx 0))

eval env (A.IfEl t t1 t2) = do
    VBit b <- eval env t
    eval env $ if b == 1 then t1 else t2

eval env (A.Let eq inn) = do
        VTup v1 v2 <- eval env eq
        eval env{ values = v2 : v1 : values env } inn

fromVTup :: Value -> [Value]
fromVTup (VTup a b) = a : fromVTup b
fromVTup         x  = [x]

toVTup :: [Value] -> Value
toVTup = foldr1 VTup

-- | Run QFT gate
runQFT :: ([QBit] -> QM [QBit]) -> A.Term -> Env -> Eval Value
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
runGate :: (QBit -> QM QBit) -> A.Term -> Env -> Eval Value
runGate g q env = do
    VQBit q' <- eval env q
    VQBit <$> lift (g q')

-- | Run gate taking two qubits
run2Gate :: ((QBit, QBit) -> QM (QBit, QBit)) -> A.Term -> Env -> Eval Value
run2Gate g q env = do
    VTup (VQBit a) (VQBit b) <- eval env q
    (p,q) <- lift (g (a,b))
    return $ VTup (VQBit p) (VQBit q)

-- | Run gate taking three qubits
run3Gate :: ((QBit, QBit, QBit) -> QM (QBit, QBit, QBit)) -> A.Term -> Env -> Eval Value
run3Gate g q env = do
    [VQBit a, VQBit b, VQBit c] <- fromVTup <$> eval env q
    toVTup . tupToList <$> lift (g (a,b,c))
        where tupToList (a,b,c) = [VQBit a, VQBit b, VQBit c]