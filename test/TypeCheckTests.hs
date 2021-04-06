{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

module TypeCheckTests where 
import Type.HM
import AST.AST
import Data.Either (lefts,rights, isLeft, isRight)
import Test.QuickCheck


--- Test that should be true

-- | Inferring 0 should be !Bit
prop_testBit = inferExp "0" === Right (TypeDup TypeBit)
-- | The inside is (!Bit, !Bit) which makes ! move outside, to !(Bit, Bit)
prop_testDupProd = inferExp "(0, 0)" === Right (TypeDup (TypeBit :>< TypeBit))

-- | Expected (?a, ?b) -o ?a
prop_testFst = inferExp "\\x.let (a,b) = x in a" === Right (TypeVar "a" :>< TypeVar "b" :=> TypeVar "a")
-- == Right ((TypeFlex "a" :>< TypeFlex "b") :=> TypeFlex "a")

-- | \x.x should have type ?a->?a, since it could be either linear or not linear.
prop_id = inferExp "\\x.x" === Right (TypeVar "a" :=> TypeVar "a")

-- | Test that the output of a linear function cannot be made duplicable ??
prop_linFunSub = expectError . typecheck . run $ "f : !(Bit -o Bit) " ++
                                                 "f x = meas (new x) " ++
                                                 "g : !Bit " ++
                                                 "g = f 0 "

-- | prop_ nested let statements
prop_NestTup = inferExp "\\x . let (a,b) = x in let (b,c) = b in (a,b,c)"  ===  Right (TypeVar "a" :>< TypeVar "b" :>< TypeVar "c" :=> TypeVar "a" :>< TypeVar "b" :>< TypeVar "c")

-- trueprop_s :: IO () 
-- trueTests = do 
--     putStrLn $ "testBit: " ++ show testBit   
    --return $ testBit && testDupProd && testFst && testId  && testLinFunSub  

--- Test that should fail (throw an exception)
expectError :: Either e t -> Property
expectError = property . isLeft

expectSuccess :: Either e t -> Property 
expectSuccess = property . isRight

-- | (\x. let(a,b) = x in a) 0 should fail, since 0 is not a product type
-- Error, SubtypeFailError
prop_LetNoProd = expectError $ inferExp "(\\x.let (a,b) = x in a) 0" -- === Left (SubtypeFailError (TypeVar "c" :>< TypeVar "d") (TypeDup TypeBit))

-- | "(\\x . if new 0 then x else x) 0" should fail, since new 0 is a qubit
prop_IfNoBit = expectError $ inferExp "(\\x . if new 0 then x else x) 0"

-- | prop_ that if statments with mismatching types of then else throws an error 
prop_MisMatchingIf = head (lefts $ runtc "q : QBit q = new 0 f : QBit f = if 1 then q else 0") === UnificationFailError TypeQBit (TypeDup TypeBit) 
-- | Test that it is not possible to create a duplicable qubit
-- ??
prop_DupQbit = expectError . typecheck .run $  "q : !QBit q = new 0"

-- | Test that a linear bit cannot be used in a nonlinear function. Should fail.
prop_LinBit = expectError . typecheck . run $ "b : Bit b = 0 " 
                            ++ "dup : !(Bit >< Bit) dup = (b,b)"

-- | Test that the output of a linear function cannot be made duplicable. Should fail.
prop_testLinFun = expectError . typecheck . run $ "f : Bit -o Bit " ++
                                                  "f x = meas (new x) " ++
                                                  "g : !Bit " ++
                                                  "g = f 0 "

-- | Test linearity of single variable, should not be possible to have two references to a linear bit. Should fail.
prop_LinRef = expectError . typecheck . run $ "b1 : Bit b1 = 0 " ++
                               "b2 : Bit b2 = b1 " ++ 
                               "b3 : Bit b3 = b1"

-- | Test a linear function can't be used twice. Should fail.
prop_UseLinFunTwice = expectError . typecheck . run $ "f : Bit -o Bit f a = 0 " ++
                                        "b2 : Bit b2 = f 1 " ++ 
                                        "b3 : Bit b3 = f 1"

-- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
prop_IfLinBit = expectSuccess . typecheck . run $ "f : Bit -o Bit f g = if g then 0 else 1"

-- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
-- fails unsuccessfully
prop_IfLinBit2 = expectSuccess . typecheck . run $ "g : Bit g = 0 " ++ 
                                    "f : Bit f = if g then 0 else 1"

-- | Test that an unlinear bit can be used as a condition in an if statement. Should succeed.
prop_IfNormalBit = expectSuccess . typecheck . run $ "g : !Bit g = 0 " ++ 
                                     "f : Bit f = if g then 0 else 1"

-- | Test a nonlinear function can be used many times. Should succeed.
prop_UseLinFun = expectSuccess . typecheck . run $ "f : !(Bit -o Bit) f a = 0 " ++
                                                   "b2 : Bit b2 = f 1 " ++ 
                                                   "b3 : Bit b3 = f 1"

uselinfun = typecheck . run $ "f : !(Bit -o Bit) f a = 0 " ++
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
prop_test = expectError . typecheck . run $ "f : a -o a f g = if g then g else 1"
-- !Bit -o !Bit

-- | Can assign a unlinear bit to a linear bit. Should succeed.
prop_LinBit2 = expectSuccess . typecheck . run $ "f : Bit f = 0"

-- | Passing a linear bit to something that expects an unlinear bit should fail
prop_linBitAsUnlinBit = expectError . typecheck . run $ "a : Bit a = 0" ++
                                     "f : !Bit -o !Bit f b = if b then 0 else 0" ++
                                     "v : !Bit v = f a"
-- | Trying to duplicate a linear tuple, should fail.
prop_DupLin = expectError . typecheck . run $ "clone : (Bit >< Bit) -o ((Bit >< Bit) >< (Bit >< Bit)) " ++
                                              "clone a = (a,a)"

-- | Higher order functions test. Should succeed.
prop_SubtypeHoF = expectSuccess $ inferExp "\\f . \\ qs . let (x,y) = qs in (f x, y)" -- == Right ((TypeFlex "a" :=> TypeFlex "b"):=> (TypeFlex "a" :><  TypeFlex "c") :=> TypeFlex "b" :>< TypeFlex "c")

-- | Simple higher order function test. Should succeed.
prop_SimpleHoF = expectSuccess $ inferExp "\\f . \\x . f x" -- == Right ((TypeFlex "a" :=> TypeFlex "b") :=> TypeFlex "a" :=> TypeFlex "b")

-- | new can receive unlinear bit. Should succeed.
prop_SimpleApp = expectSuccess . typecheck . run $ "b : QBit b = new 0"

-- | Can't give QBit to !QBit -o !QBit. Should fail.
prop_DupFunLinArg = expectError . typecheck . run $ "q : QBit q = new 0 " ++
                                                    "f : !QBit -o !QBit f x = x " ++
                                                    "main : !QBit main = f q"
-------------- ERRORS -----------------


-- | Test that a linear bit cannot be used in a non-linear way in an if statement. Should fail.
prop_IfUnLin = expectError . typecheck . run $ "f : Bit -o Bit f g = if g then g else 1"

--testL
-- / !Bit -o !Bit :> Bit -o Bit this gives fine  if 0 then 1 else 0  Bit, !Bit, ?Bit

-- unify !Bit Bit  --> !Bit == Bit, parent
-- unify Bit !Bit  --> Bit 

prop_FunInTup = expectSuccess $ inferExp "(\\x.(x,x,x,x,0,1)) new"

prop_IfGeneral = expectSuccess $ inferExp "\\x. if x then 0 else 1"

-- | Should succeed: Right !Bit ⊸ !(Bit ⊗ Bit)
prop_DupIf = expectSuccess $ inferExp "\\x.if x then (x,x) else (x,x)"

prop_IfOnce =  expectSuccess $ inferExp "\\x.if x then 0 else 0" -- a~?Bit=>[a/?Bit] ?Bit -o !Bit
prop_IfSig = expectSuccess $ typecheck . run $ "f   : !Bit -o !Bit " 
                           ++ "f x = if x then 0 else 1"
-- Should work 
-- prop_EqDup = expectSuccess $ inferExp "\\x.(x,0)"



-- Need return for quickCheckAll
return []
runTests = $quickCheckAll