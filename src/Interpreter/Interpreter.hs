{-# language LambdaCase #-}

module Interpreter.Interpreter where

import System.IO

import qualified Data.Map as M
import Control.Monad.Except
import Control.Monad.Reader
import Data.List
import Control.Monad.Identity
import qualified FunQ as Q
import Control.Monad.State
import Parser.Abs as Abs
    ( Gate(GS, GH, GX, GY, GZ, GI, GT, GCNOT, GTOF, GSWP, GFRDK),
      Type,
      Bit(BOne, BZero),
      Program )
import qualified AST.AST as A
import Parser.Par              (pProgram, myLexer)

-- TODO:
-- fixa errors 
-- teleportera 1, och få 1
-- eval för custom run gate (vart/hur definierar ens användaren sina egna gates?)
-- generell run gate???
-- show för VFunc? 
-- main driver: egen fil? turtle och haskelline? 
-- test suite

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
}


instance Show Value where
    show (VBit b)    = show b
    show (VTup bs)   = show bs
    show (VQBit q)   = show q
    show  VUnit      = "*"
    show (VFunc t e) = "nope"
    -- show (VFunc vs e) = case runCheck (typecheck (Abs t e)) of
    --     Right t' -> show t'

-- Main function in interpreter (which we export)
interpret :: [A.Function] -> Eval Value
interpret fs = do
    -- Create context 
    let ctx = createctxs M.empty fs
    -- Eval main function
    let mainTerm = getMainTerm ctx
    -- Return the return value from main
    eval ctx mainTerm

createctxs :: Sig -> [A.Function] -> Ctx
createctxs sig fs = Ctx { functions = M.fromList [(s, t) | (A.Func s _ t) <- fs],
                            values = []}

getMainTerm :: Ctx -> A.Term
getMainTerm ctx = case M.lookup "main" (functions ctx) of
    Just term -> term
    Nothing   -> error "No main function" -- TODO: use our own errors

data Value
    = VBit Q.Bit
    | VQBit Q.QBit
    | VUnit
    | VTup [Value]
    | VFunc [Value] A.Term -- could [Value] be removed?

eval :: Ctx -> A.Term -> Eval Value     
eval ctx = \case
    A.Idx j -> return $ values ctx !! fromInteger j

    A.QVar s -> case M.lookup s (functions ctx) of
        Just t  -> eval ctx t
        Nothing -> error "qbit?"  -- Must be a free variable? qbit?

    A.Bit BZero -> return $ VBit 0
    A.Bit BOne -> return $ VBit 1

    A.Gate g -> error "Using gate on nothing" -- TODO: correct errors

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

    A.Abs e -> return $ VFunc (values ctx) e

    A.Void -> return VUnit

    A.New -> error "New should not be found by itself"
    
    A.Meas -> error "Meas should not be found by itself"


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


-- Should be in a main pipeline file
run :: String -> IO ()
run fileName = do
    prg <- readFile fileName
    program <- parse prg
    res <- Q.run $ runExceptT $ interpret (A.toIm program)
    case res of
        Left err -> do
            putStrLn "INTERPRETER ERROR"
            error ""
        Right i -> do
            putStrLn $ "Result " ++ show i

parse :: String -> IO Program
parse s = case pProgram (myLexer s) of
  Left err -> do
    putStrLn "SYNTAX ERROR"
    putStrLn err
    error "SYNTAX ERROR"
  Right prg -> do
    return prg

