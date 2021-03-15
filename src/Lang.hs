{-# LANGUAGE GADTs #-}
module Lang where

import FunQ (QM, Bit, run, new, QBit, measure, hadamard)


type Var = String
data Term a where
    New :: Term Bit -> Term QBit
    Zero :: Term Bit
    One :: Term Bit
    Measure :: Term QBit -> Term Bit
    -- Todo: Use Gate instead of hadamard.
    Hadamard :: Term QBit -> Term QBit
    If :: Term Bit -> Term a -> Term a -> Term a

    Abstraction :: (Term a -> Term b) -> Term (a->b)
    Application :: Term (a->b) -> Term a -> Term b

    Product :: Term a -> Term b -> Term (a, b)
    Let :: Term (a, b) -> (Term a -> Term b -> Term c) -> Term c
    
    

data Gate = H | Not 

eval' term = run $ eval term

eval :: Term a -> QM a 
eval (New a) = evalNew a
eval (Measure qbitTerm) = evalMeasure qbitTerm
eval (Hadamard qbitTerm) = evalHadamard qbitTerm
eval (If termBit termLeft termRight) = evalIf termBit termLeft termRight
eval Zero = return 0
eval One = return 1
eval (Application (Abstraction f) arg) = evalApplication f arg
eval (Let (Product a b) f) = evalLet f a b 

evalNew :: Term Bit -> QM QBit
evalNew Zero = new 0
evalNew One = new 1

evalMeasure :: Term QBit -> QM Bit
evalMeasure qbitTerm = do
    let qmQbit = eval qbitTerm
    qbit <- qmQbit
    measure qbit

evalHadamard :: Term QBit -> QM QBit
evalHadamard qbitTerm = do
    let qmQbit = eval qbitTerm
    qbit <- qmQbit
    hadamard qbit

evalIf :: Term Bit -> Term a -> Term a -> QM a
evalIf termBit termLeft termRight = do
    let qmBit = eval termBit
    bit <- qmBit
    let left = eval termLeft
    let right = eval termRight
    if bit == 1 then left else right

evalApplication :: (a -> Term b) -> a -> QM b
evalApplication f arg = eval (f arg)

evalLet f a b = eval $ f a b

qbit0 :: Term QBit
qbit0 = New Zero 

program :: Term Bit
program = Measure qbit0

program2 = Measure $ Hadamard qbit0

-- Reverse
program3 = If program Zero One

hadamardTwice :: Term (QBit -> QBit)
hadamardTwice = Abstraction (Hadamard . Hadamard)

-- Should be zero at all times.
program4 :: Term Bit
program4 = Measure $ Application hadamardTwice qbit0

zeroAndOne = Product Zero One
program5 = Let zeroAndOne (\a b -> If a a b) --(\x y -> if x then one else zero)

result = eval' program
result2 = eval' program2
result3 = eval' program3
result4 = eval' program4
result5 = eval' program5