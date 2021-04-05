module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
    ( MonadTrans(lift), ExceptT, MonadError(throwError), join )
import qualified FunQ as Q
import Control.Monad.State (StateT, MonadState (get, put), evalStateT, modify)
import Data.Functor ( (<&>) )
import Parser.Abs as Abs
    ( Gate(GS, GH, GX, GY, GZ, GI, GT, GCNOT, GTOF, GSWP, GFRDK, GQFT),
      Bit(BOne, BZero) )
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
type Eval a = StateT Env (ExceptT ValueError Q.QM) a

-- | Environment type, stores bound variables & functions
data Env = Env {
      values    :: [Value]
    , functions :: Sig
    , count     :: Int
} deriving Show

instance Show Value where
    show (VBit b)    = show b
    show (VTup a b)   = "(" ++ show a ++ "," ++ show b ++ ")"
    show (VQBit q)   = show q
    show VUnit       = "*"
    show (VFunc _ t) = "Function " ++ show t

-- | Main function in interpreter (exported)
interpret :: [A.Function] -> ExceptT ValueError Q.QM Value
interpret fs = do
    let env = createEnv fs
    m <- getMainTerm env
    evalStateT (eval m) env

-- | Creates an environment from a list of functions. 
createEnv :: [A.Function] -> Env
createEnv fs = Env { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                     values    = [],
                     count     = 0 }

-- | Fetches program main function
getMainTerm :: Env -> ExceptT ValueError Q.QM A.Term
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
eval :: A.Term -> Eval Value
eval t = do
    case t of
        A.Idx j -> do
            env <- get
            printE $ "values_idx: " ++ show (values env)
            let val = values env !! fromIntegral j
            
            
            return val
            -- return $ values env !! fromIntegral j
            -- let vals = values env
            -- let val = values env !! fromIntegral j
            -- pickOut vals j

        A.Fun s -> do
            env <- get
            case M.lookup s (functions env) of
                Just t  -> eval t
                Nothing -> throwError $ NotFunction $ "Function " ++ show s ++ " is not defined"

        A.Bit BZero -> do
            env <- get
            printE $ "values_b: " ++ show (values env)
            
            
            return $ VBit 0
        A.Bit BOne -> do
            env <- get
            printE $ "values_bone: " ++ show (values env)
            
            
            return $ VBit 1

        A.Tup t1 t2 -> do
            env <- get
            v1 <- eval t1
            v2 <- eval t2
            printE $ "values_tup: " ++ show (values env)
            
            
            return $ VTup v1 v2

        A.App e1 e2 -> case e1 of
            A.Gate g -> do
                -- printE $ "e2: " ++ show e2
                case g of
                    Abs.GH    -> runGate  Q.hadamard e2
                    Abs.GX    -> runGate  Q.pauliX e2
                    Abs.GY    -> runGate  Q.pauliY e2
                    Abs.GZ    -> runGate  Q.pauliZ e2
                    Abs.GI    -> runGate  Q.identity e2
                    Abs.GT    -> runGate  Q.phasePi8 e2
                    Abs.GS    -> runGate  Q.phase e2
                    Abs.GCNOT -> run2Gate Q.cnot e2
                    Abs.GTOF  -> run3Gate Q.toffoli e2
                    Abs.GSWP  -> run2Gate Q.swap e2
                    Abs.GFRDK -> run3Gate Q.fredkin e2
                    Abs.GQFT  -> runQFT   Q.qft e2
            A.New -> do
                VBit b' <- eval e2
                
                
                liftT $ Q.new b' <&> VQBit
            A.Meas -> do
                VQBit q' <- eval e2
                
                
                liftT $ Q.measure q' <&> VBit
            _ -> do
                env <- get
                v2 <- eval e2
                VFunc v1 a <- eval e1
                put env { values = v2 : v1 ++ values env }
                -- insertVal v1
                -- insertVal [v2]
                -- printE $ "values: " ++ show (values env)
                eval a

        A.IfEl bit l r -> do
            VBit b <- eval bit
            eval $ if b == 1 then l else r

        A.Let eq inn -> do
            env <- get
            VTup x1 x2 <- eval eq
            put env {values = x1 : x2 : values env}
            -- insertVal [x2]
            -- insertVal [x1]
            eval inn

        A.Abs e  -> do
            env <- get
            printE $ "counter: " ++ show (count env)
            put env{count = count env + 1}
            printE $ "counter: " ++ show (count env)
            return $ VFunc (values env) e
        A.Unit   -> return VUnit
        A.Gate g -> do
            env <- get
            return $ VFunc (values env) (A.App (A.Gate g) (A.Idx 0))
        A.New    -> do
            env <- get
            return $ VFunc (values env) (A.App A.New (A.Idx 0))
        A.Meas   -> do
            env <- get
            return $ VFunc (values env) (A.App A.Meas (A.Idx 0))

fromVTup :: Value -> [Value]
fromVTup (VTup a b) = a : fromVTup b
fromVTup         x  = [x]

toVTup :: [Value] -> Value
toVTup = foldr1 VTup

-- | Eval monad print
printE :: Show a => a -> Eval ()
printE = liftT . Q.io . print

liftT :: Q.QM a -> Eval a
liftT = lift . lift

-- | Run QFT gate
runQFT :: ([Q.QBit] -> Q.QM [Q.QBit]) -> A.Term -> Eval Value
runQFT g q = do
    res <- eval q
    case res of
        (VQBit q') ->
            liftT (g [q']) <&> VQBit . head
        vt@(VTup _ _) -> do
            b <- liftT $ g (unValue (fromVTup vt))
            return $ toVTup $ map VQBit b
        where unValue []            = []
              unValue (VQBit q:qss) = q : unValue qss

-- | Run gate taking one qubit
runGate :: (Q.QBit -> Q.QM Q.QBit) -> A.Term -> Eval Value
runGate g q = do
    env <- get
    VQBit q' <- eval q
    printE $ "values_runG: " ++ show (values env)
    
    
    VQBit <$> liftT (g q')

-- | Run gate taking two qubits
run2Gate :: ((Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit)) -> A.Term -> Eval Value
run2Gate g q = do
    env <- get
    VTup (VQBit a) (VQBit b) <- eval q
    (p,q) <- liftT (g (a,b))
    printE $ "values_run2G: " ++ show (values env)
    
    
    return $ VTup (VQBit p) (VQBit q)

-- | Run gate taking three qubits
run3Gate :: ((Q.QBit, Q.QBit, Q.QBit) -> Q.QM (Q.QBit, Q.QBit, Q.QBit)) -> A.Term -> Eval Value
run3Gate g q = do
    [VQBit a, VQBit b, VQBit c] <- fromVTup <$> eval q
    toVTup . tupToList <$> liftT (g (a,b,c))
        where tupToList (a,b,c) = [VQBit a, VQBit b, VQBit c]