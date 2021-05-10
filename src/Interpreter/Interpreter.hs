{-# language LambdaCase #-}

module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.State
    ( StateT, MonadState(get), evalStateT, modify )
import qualified FunQ as Q
import Lib.QM (link)
import qualified AST.AST as A

data ValueError
    = NotFunction String
    | Fail String

instance Show ValueError where
    show (NotFunction s) = "Function " ++ s ++ " is not defined"
    show (Fail s) = s


type Sig = M.Map String A.Term
type FunctionValues = M.Map String Value
type Eval = ExceptT ValueError (StateT FunctionValues Q.QM)

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
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup Value Value
    | VAbs [Value] A.Type A.Term
    | VNew
    | VMeas
    | VGate A.Gate

-- | Main function in interpreter (exported)
interpret :: [A.Function] -> Q.QM (Either ValueError Value)
interpret fs = evalStateT (runExceptT main) M.empty
    where env  = createEnv fs
          main = getMainTerm env >>= eval env

-- | Creates an environment from a list of functions. 
createEnv :: [A.Function] -> Env
createEnv fs = Env { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                     values = []}

-- | Fetches program main function
getMainTerm :: Env -> Eval A.Term
getMainTerm env = case M.lookup "main" (functions env) of
    Just term -> return term
    Nothing   -> throwError $ Fail "Main function not defined"

-- | Term evaluator
eval :: Env -> A.Term -> Eval Value
eval env (A.Bit A.BZero) = return $ VBit 0
eval env (A.Bit A.BOne)  = return $ VBit 1
eval env (A.Abs t e)     = return $ VAbs (values env) t e
eval env (A.Unit)        = return VUnit
eval env (A.Gate g)      = return $ VGate g
eval env (A.New)         = return VNew
eval env (A.Meas)        = return VMeas

eval env (A.Idx j) = if j >= fromIntegral (length (values env)) 
    then throwError (Fail ("Index" ++ show j ++ "is too large"))
    else return $ values env !! fromIntegral j

eval env (A.Fun s) = do 
    case M.lookup s (functions env) of
        Just t  -> do
            fs <- get
            case M.lookup s fs of
                (Just v) -> return v
                Nothing  -> do
                    v <- eval env t
                    modify (M.insert s v)
                    return v
        Nothing -> throwError $ NotFunction s

eval env (A.Tup t1 t2) = do
    v1 <- eval env t1
    v2 <- eval env t2
    return $ VTup v1 v2

eval env (A.App t1 t2) = do
    case t1 of
        A.Gate g -> case g of
            A.GH      -> runGate  Q.hadamard t2 env
            A.GX      -> runGate  Q.pauliX t2 env
            A.GY      -> runGate  Q.pauliY t2 env
            A.GZ      -> runGate  Q.pauliZ t2 env
            A.GI      -> runGate  Q.identity t2 env
            A.GT      -> runGate  Q.phasePi8 t2 env
            A.GS      -> runGate  Q.phase t2 env
            A.GCNOT   -> run2Gate Q.cnot t2 env
            A.GTOF    -> run3Gate Q.toffoli t2 env
            A.GSWP    -> run2Gate Q.swap t2 env
            A.GFRDK   -> run3Gate Q.fredkin t2 env
            A.GQFT n  -> runQFT   (Q.qft n) t2 env
            A.GQFTI n -> runQFT   (Q.qftDagger n) t2 env
            A.GCR n   -> run2Gate (`Q.cphase` (1/(n*2))) t2 env
            A.GCRI n  -> run2Gate (`Q.cphase` (-1/(n*2))) t2 env
            A.GCCR n  -> run3Gate (`Q.ccphase` (1/(n*2))) t2 env
            A.GCCRI n  -> run3Gate (`Q.ccphase` (-1/(n*2))) t2 env

        A.New -> do
            VBit b' <- eval env t2
            lift $ lift $ VQBit <$> Q.new b'
        A.Meas -> do
            VQBit q' <- eval env t2
            lift $ lift $ VBit <$> Q.measure q'
        _ -> do
            v2 <- eval env t2
            v1 <- eval env t1
            case v1 of
                VAbs vs _ a -> eval env{ values = v2 : vs ++ values env } a
                VNew -> eval env{ values = v2 : values env } (A.App A.New (A.Idx 0))
                VMeas -> eval env{ values = v2 : values env } (A.App A.Meas (A.Idx 0))
                (VGate g) -> eval env{ values = v2 : values env } (A.App (A.Gate g) (A.Idx 0))
                _ -> throwError $ Fail $ "Can't apply " ++ show v1 ++ " with " ++ show v2

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
runQFT :: ([Q.QBit] -> Q.QM [Q.QBit]) -> A.Term -> Env -> Eval Value
runQFT g q env = do
    res <- eval env q
    case res of
        (VQBit q') ->
            lift $ VQBit . head <$> lift (g [q'])
        vt@(VTup _ _) -> do
            b <- lift $ lift $ g (unValue (fromVTup vt))
            return $ toVTup $ map VQBit b
        where unValue []            = []
              unValue (VQBit q:qss) = q : unValue qss

-- | Run gate taking one qubit
runGate :: (Q.QBit -> Q.QM Q.QBit) -> A.Term -> Env -> Eval Value
runGate g q env = do
    VQBit q' <- eval env q
    lift $ VQBit <$> lift (g q')

-- | Run gate taking two qubits
run2Gate :: ((Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit)) -> A.Term -> Env -> Eval Value
run2Gate g q env = do
    VTup (VQBit a) (VQBit b) <- eval env q
    (p,q) <- lift $ lift (g (a,b))
    return $ VTup (VQBit p) (VQBit q)

-- | Run gate taking three qubits
run3Gate :: ((Q.QBit, Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit, Q.QBit)) -> A.Term -> Env -> Eval Value
run3Gate g q env = do
    [VQBit a, VQBit b, VQBit c] <- fromVTup <$> eval env q
    lift $ toVTup . tupToList <$> lift (g (a,b,c))
        where tupToList (a,b,c) = [VQBit a, VQBit b, VQBit c]