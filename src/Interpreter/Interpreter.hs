{-# language LambdaCase #-}

module Interpreter.Interpreter where

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Control.Monad.Identity
import qualified FunQ as Q
import Control.Monad.State
import Parser.Abs as Abs
import qualified AST.AST as A
import Parser.Par              (pProgram, myLexer)

type Named = Int
data Error
    = Mismatch Type Type
    | NotInScope Named
    | NotFunction Type
    | NotProduct Type
    | NotValueType
    | ParseError String
    | Fail String

type Sig = M.Map String A.Term

type Eval a = ExceptT Error Q.QM a

data Ctx = Ctx {
      values :: [Value]
    , functions :: Sig      -- Maps function identifiers to terms
    -- , variables :: Map Ident 
}

data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup [Value]
    | VFunc [Value] A.Term

-- instance Show Value where
--     show (VBit b)    = show b
--     show (VTup bs)   = show bs
--     show (VQBit q)   = show q
--     show VUnit       = "*"
--     show (VFunc t _ e) = case runCheck (typecheck (Abs t e)) of
--         Right t' -> show t'

-- Main function in interpreter (which we export )
interpret :: [A.Function] -> Eval Value
interpret fs = do 
    -- Create context 
    let ctx = createctxs (M.empty) fs
    -- Eval main function
    let mainTerm = getMainTerm ctx
    -- Return the return value from main
    val <- eval ctx mainTerm
    return val

createctxs :: Sig -> [A.Function] -> Ctx
createctxs sig fs = undefined --  Ctx {functions = map insertM fs} -- Ctx { functions = (map . uncurry) (M.insert sig) [(s, t) | (A.Func s _ t) <- fs]}
    where insertM env k a = M.insert k a env
--ctx {functions = M.insert s t}
-- Func String Type Term 
-- (Func fname _ fterm)


--  cxt{ functions = Map.insert fname fterm env }

getMainTerm :: Ctx -> A.Term
getMainTerm ctx = case M.lookup "main" (functions ctx) of 
    Just term -> term 
    Nothing   -> error "No main function" -- TODO: use our own errors

eval :: Ctx -> A.Term -> Eval Value     -- ??
eval ctx = \case
    A.Idx j -> return $ values ctx !! fromInteger j

    -- A.QVar s -> qbits?
        
    A.Bit BZero -> return $ VBit 0
    A.Bit BOne -> return $ VBit 1
    
    A.Gate g -> error "Using gate on nothing" -- TODO: correct errors
    
    A.App (A.Gate g) q -> case g of
         Abs.GH    -> runGate Q.hadamard q ctx
         Abs.GX    -> runGate Q.pauliX q ctx
         Abs.GY    -> runGate Q.pauliY q ctx
         Abs.GZ    -> runGate Q.pauliZ q ctx
         Abs.GI    -> runGate Q.identity q ctx
         Abs.GT    -> runGate Q.tdagger q ctx
         Abs.GS    -> runGate Q.phase q ctx
        --Abs.GCNOT -> runGate Q.cnot q ctx
        --Abs.GTOF  -> runGate Q.toffoli q ctx
        --Abs.GSWP  -> runGate Q.swap q ctx
        --Abs.GFRDK -> runGate Q.fredkin q ctx
        -- GGate GateIdent


runGate :: (Q.QBit -> Q.QM Q.QBit) -> A.Term -> Ctx -> Eval Value
runGate g q ctx = do
    VQBit q' <- eval ctx q
    VQBit <$> lift (g q')
    

-- runCNOT :: A.Term -> Ctx -> Eval Value
-- runCNOT q ctx = do
--     VTup q' <- eval ctx q
--     VTup <$> lift [fst (Q.cnot q), snd (Q.cnot q)]
    -- A.Void -> return VUnit

    -- A.Tup bs -> VTup <$> mapM (eval ctx) bs

    -- one = \f.\x.f x
    -- suc = \n.\f.\x.n f (f x)
    -- eval $ suc one
    -- suc[n := one] -\beta> 
    -- \f.\x. (\f.\x.f x ) f (f x)
    -- \f.\x. f (f x)

    -- A.Abs e -> return $ VFunc (values ctx) e

    -- A.App A.New b -> do
        -- VBit b' <- eval ctx b
        -- q <- lift $ Q.new b'
        -- return $ VQBit q

   -- A.App A.Meas q -> do
   --     VQBit q' <- eval ctx q
   --     b <- lift $ Q.measure q' 
   --     return $ VBit b
--

   --     
   -- A.App e1 e2 -> do   -- EXAMPLE: f 10   0 -> 10  f --> \0
   --     VFunc e a <- eval ctx e1
   --     v <- eval ctx e2
   --     eval ctx{values = v : values} a
--
   -- A.IfEl bit l r -> do
   --     VBit b <- eval ctx bit 
   --     eval ctx $ if b == 1 then l else r
   -- 
   -- A.Let eq inn -> do 
   --     VTup [x1, x2] <- eval ctx eq 
   --     eval ctx{values = x2 : x1 : values} inn
--
   -- _ -> throwError $ Fail "not implemented"
    
--  case t of
--          Bit b -> return $ VBit b

-- | A term in our intermediatary representation.
-- data Term
--    = Idx  Integer       -- bound
--    | QVar String        -- free
--    | Bit  Bit
--    | Gate Gate
--    | Tup  [Term]
--    | App  Term Term
--    | IfEl Term Term Term
--    | Let Term Term
--    | Abs  Term
--    | New
--    | Meas
--    | Void

    
-- Should be in a main pipeline file
run :: String -> IO Value 
run prg = do 
    program <- parse prg
    i <- interpret (A.toIm program)
    return (VBit 0)

parse :: String -> IO ()
parse s = case pProgram (myLexer s) of
  Left err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    error "SYNTAX ERROR"
  Right prg -> do 
    putStr "hej" -- prg
