module TypeCheckTests where 
import Type.HM
import AST.AST
import Data.Either (lefts,rights)

--- Test that should be true

-- | Inferring 0 should be !Bit
testBit = inferExp "0" == Right (TypeDup TypeBit)
-- | The inside is (!Bit, !Bit) which makes ! move outside, to !(Bit, Bit)
testDupProd = inferExp "(0, 0)" == Right (TypeDup (TypeBit :>< TypeBit))

-- | Expected (?a, ?b) -o ?a
testFst = inferExp "\\x.let (a,b) = x in a" -- == Right ((TypeFlex "a" :>< TypeFlex "b") :=> TypeFlex "a")

-- | \x.x should have type ?a->?a, since it could be either linear or not linear.
testId = inferExp "\\x.x" -- == Right (TypeFlex "a" :=> TypeFlex "a")

-- | Test that the output of a linear function cannot be made duplicable ??
testLinFunSub = null (lefts $ runtc $ "f : !(Bit -o Bit) " ++
                                      "f x = meas (new x) " ++
                                      "g : Bit " ++
                                      "g = f 0 ")
-- | Test nested let statements
testNestTup = inferExp "\\x . let (a,b) = x in let (b,c) = b in (a,b,c)" -- ==  Right (TypeFlex "a" :>< TypeFlex "b" :>< TypeFlex "c" :=> TypeFlex "a" :>< TypeFlex "b" :>< TypeFlex "c")

trueTests :: IO Bool 
trueTests = undefined 
    --return $ testBit && testDupProd && testFst && testId  && testLinFunSub  

--- Test that should fail (throw an exception)

-- | (\x. let(a,b) = x in a) 0 should fail, since 0 is not a product type
testLetNoProd = inferExp "(\\x.let (a,b) = x in a) 0" -- == Left (SubtypeFailError (TypeFlex "c" :>< TypeFlex "d") (TypeDup TypeBit))

-- | "(\\x . if new 0 then x else x) 0" should fail, since new 0 is a qubit
testIfNoBit = inferExp "(\\x . if new 0 then x else x) 0" == Left (SubtypeFailError (TypeDup TypeBit) TypeQBit)

-- | Test that if statments with mismatching types of then else throws an error 
testMisMatchingIf = head (lefts $ runtc "q : QBit q = new 0 f : QBit f = if 1 then q else 0") == UnificationFailError TypeQBit (TypeDup TypeBit) 
-- | Test that it is not possible to create a duplicable qubit
testDupQbit = head (lefts $ runtc "q : !QBit q = new 0") == UnificationFailError (TypeDup TypeQBit) TypeQBit

-- | Test that a linear bit cannot be used in a nonlinear function
testLinBit = head $ lefts $ runtc $ "b : Bit b = 0 " 
                                 ++ "dup : !(Bit >< Bit) dup = (b,b)"

-- | Test that the output of a linear function cannot be made duplicable. Should fail.
testLinFun = typecheck . run $ "f : Bit -o Bit " ++
                               "f x = meas (new x) " ++
                               "g : !Bit " ++
                               "g = f 0 "

-- | Test linearity of single variable, should not be possible to have two references to a linear bit. Should fail.
testLinRef = typecheck . run $ "b1 : Bit b1 = 0 " ++
                               "b2 : Bit b2 = b1 " ++ 
                               "b3 : Bit b3 = b1"

-- | Test a linear function can't be used twice. Should fail.
testUseLinFunTwice = typecheck . run $ "f : Bit -o Bit f a = 0 " ++
                                       "b2 : Bit b2 = f 1 " ++ 
                                       "b3 : Bit b3 = f 1"

-- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
testIfLinBit = typecheck . run $ "f : Bit -o Bit f g = if g then 0 else 1"

-- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
testIfLinBit2 = typecheck . run $ "g : Bit g = 0 " ++ 
                                  "f : Bit f f = if g then 0 else 1"

-- | Test that an unlinear bit can be used as a condition in an if statement. Should succeed.
testIfNormalBit = typecheck . run $ "g : !Bit g = 0 " ++ 
                                    "f : Bit f = if g then 0 else 1"

-- | Test a nonlinear function can be used many times. Should succeed.
testUseLinFun = typecheck . run $ "f : !(Bit -o Bit) f a = 0 " ++
                               "b2 : Bit b2 = f 1 " ++ 
                               "b3 : Bit b3 = f 1"

-- 1. g = !a
-- 2. if !a then !a else 1
-- 3. unify !a with !Bit -> a = Bit
-- 4. if !Bit then !Bit else 1
-- 5. unify Bit -o Bit with !Bit -o !Bit
-- 6
-- Unify !a !Bit -> a = Bit
-- Unify Bit !Bit
testtest = typecheck . run $ "f : a -o a f g = if g then g else 1"
-- !Bit -o !Bit

-- | Can assign a unlinear bit to a linear bit. Should succeed.
testLinBit2 = typecheck . run $ "f : Bit f = 0"

-- | Passing a linear bit to something that expects an unlinear bit should fail
linBitAsUnlinBit = typecheck . run $ "a : Bit a = 0" ++
                                     "f : !Bit -o !Bit f b = if b then 0 else 0" ++
                                     "v : !Bit v = f a"
-- | Trying to duplicate a linear tuple, should fail.
testDupLin = typecheck . run $ "clone : (Bit >< Bit) -o ((Bit >< Bit) >< (Bit >< Bit)) " ++
                               "clone a = (a,a)"

-- | Higher order functions test. Should succeed.
testSubtypeHoF = inferExp "\\f . \\ qs . let (x,y) = qs in (f x, y)" -- == Right ((TypeFlex "a" :=> TypeFlex "b"):=> (TypeFlex "a" :><  TypeFlex "c") :=> TypeFlex "b" :>< TypeFlex "c")

-- | Simple higher order function test. Should succeed.
testSimpleHoF = inferExp "\\f . \\x . f x" -- == Right ((TypeFlex "a" :=> TypeFlex "b") :=> TypeFlex "a" :=> TypeFlex "b")

-- | new can receive unlinear bit. Should succeed.
testSimpleApp = typecheck . run $ "b : QBit b = new 0"

-- | Can't give QBit to !QBit -o !QBit. Should fail.
testDupFunLinArg = typecheck . run $ "q : QBit q = new 0 " ++
                            "f : !QBit -o !QBit f x = x " ++
                            "main : !QBit main = f q"
-------------- ERRORS -----------------


-- | Test that a linear bit cannot be used in a non-linear way in an if statement. Should fail.
testIfUnLin = typecheck . run $ "f : Bit -o Bit f g = if g then g else 1"

--testL
-- / !Bit -o !Bit :> Bit -o Bit this gives fine  if 0 then 1 else 0  Bit, !Bit, ?Bit

-- unify !Bit Bit  --> !Bit == Bit, parent
-- unify Bit !Bit  --> Bit 

testFunInTup = inferExp "(\\x.(x,x,x,x,0,1)) new"

testIfGeneral = inferExp "\\x. if x then 0 else 1"