{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedStrings      #-}

module TypeCheckTests where 
import Type.TypeChecker
import Test.QuickCheck (quickCheckAll, (===), Property, Testable(property), quickCheck, Arbitrary(..), Gen(..), frequency, elements, generate, sample, withMaxSuccess, (==>), expectFailure)
import Data.Either (lefts,rights, isLeft, isRight)
import AST.AST ( Type(TypeDup, (:=>), (:><)) )
import Control.Monad.Except
import Control.Monad.Reader 
import Control.Monad.State 
import qualified Data.Set as Set
import qualified Data.Map as Map

instance Arbitrary Type where
    arbitrary = frequency 
              [ (6, elements ["Bit", "T", "QBit"])
              , (1, (:=>) <$> arbitrary <*> arbitrary)
              , (1, (:><) <$> arbitrary <*> arbitrary)
              , (2, TypeDup . debangg   <$> arbitrary)
              ]

-- | When typecheck is expected to succeed.
expectSuccess :: Either e t -> Property 
expectSuccess = property . isRight

--- When typecheck is expected to fail.
expectError :: Either e t -> Property
expectError = property . isLeft

testSup :: Check Type -> Maybe Type
testSup f = case runCheck f of
    Right t -> Just t
    Left _  -> Nothing

infErr = Left . TError "MEGA URK"

prop_ArgId = expectSuccess $ tcStr "f : !(Bit -o Bit) f x = x"
prop_LamId = expectSuccess $ tcStr "f : !(Bit -o Bit) f = \\x:Bit.x"
prop_LamConst0 = expectSuccess $ tcStr "f: !(Bit -o Bit) f = \\a:Bit.0"
prop_Create0Qbit = expectSuccess $ tcStr "f: !(T -o QBit) f = \\x:T. new 0"
prop_Create0Qbit2 = expectSuccess $ tcStr "f: T -o QBit f = \\x:T. new 0"
prop_NullaryBit = expectSuccess $ tcStr "f: Bit f = 0"
prop_NullaryBit2 = expectSuccess $ tcStr "f: !Bit f = 0"
prop_NullaryQBit = expectError $ tcStr "f: !QBit f = new 0"
prop_NullaryQBit2 = expectSuccess $ tcStr "f: QBit f = new 0"

prop_IfGenTyp = expectSuccess $ tcStr "f: Bit f = ((\\a:Bit.\\b:!Bit.if 1 then a else b)0)0"

prop_topLin = expectError $ tcStr $
    "f : QBit f = new 0 " ++
    "g : QBit g = f " ++
    "main : QBit >< QBit main = (f,g)"

prop_linIf = expectSuccess $ tcStr $
    "q : QBit q = new 0 " ++
    "main : QBit main = if 0 then q else q"

prop_linIf2 = expectError $ tcStr $ "q : QBit q = new 0 " ++ "main : QBit main = if measure q then q else new 1"

prop_supremumBits = testSup (supremum "Bit" "!Bit") === Just "Bit"
prop_supremumBits2 = testSup (supremum "!Bit" "Bit") === Just "Bit"
prop_supremumCom t1 t2 = testSup (supremum t1 t2) === testSup (supremum t2 t1)
prop_infimumCom t1 t2 = testSup (infimum t1 t2) === testSup (infimum t2 t1)
prop_supremumAss t1 t2 t3 = testSup (assTest supremum t1 t2 t3) === testSup (assTest supremum t2 t3 t1)
prop_infimumAss t1 t2 t3 = testSup (assTest infimum t1 t2 t3) === testSup (assTest infimum t2 t3 t1)
prop_supremumIdp t = testSup (supremum t t) === Just t
prop_infimumIdp t = testSup (infimum t t) === Just t

-- t1 = !(T -o !QBit)
-- t2 = !(T -o QBit)
-- t1 <: t2 = true
-- sup t1 t2 =? T -o QBit
-- A <: B ==> supremum A B == B
prop_subSup t1 t2 = t1 <: t2 ==> (testSup (supremum t1 t2) === Just t2)

-- A <: B ==> infimum A B == A
prop_subInf t1 t2 = t1 <: t2 ==> (testSup (infimum t1 t2) === Just t1)

x = testSup (supremum "Bit -o QBit" "!Bit -o QBit")

assTest :: Monad m => (t1 -> t2 -> m t2) -> t1 -> t2 -> t1 -> m t2
assTest f a b c = f a b >>= f c


-- | The inside is (!Bit, !Bit) which makes ! move outside, to !(Bit, Bit)
prop_testDupProd = expectSuccess $ tcStr "main: !(Bit >< Bit) main = (0, 0)" 

-- | Pick out a bit from product using let
prop_testFst = expectSuccess $ tcStr "main : !(Bit >< T) -o !Bit main x = let (a,b) = x in a"

-- | Identity for !Bit should work
prop_id = expectSuccess $ tcStr "x : !Bit -o !Bit x = \\x:!Bit. x"

-- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
prop_ifLinBit = expectSuccess $ tcStr "x : Bit -o Bit x b = if b then 0 else 1"

-- | Test that a (outer level) linear bit can be used as a condition in an if statement. Should succeed.
-- fails unsuccessfully
prop_ifLinBit2 = expectSuccess $ tcStr $ "g: Bit g = 0 " ++
                                    "f : Bit f = if g then 0 else 1"

-- | Test that an unlinear bit can be used as a condition in an if statement. Should succeed.
prop_ifNormalBit = expectSuccess $ tcStr $ "g : !Bit g = 0"
                                        ++ "f : Bit f = if g then 0 else 1"

-- | Test a nonlinear function can be used many times. Should succeed.
prop_useLinFun = expectSuccess . tcStr $ "f : !(Bit -o Bit) f a = 0 " ++
                                         "b2 : Bit b2 = f 1 " ++ 
                                         "b3 : Bit b3 = f 1"

-- | Can assign a unlinear bit to a linear bit. Should succeed.
prop_unLinBit = expectSuccess . tcStr $ "f : Bit f = 0"

-- | Higher order functions test. Should succeed.
prop_subtypeHoF = expectSuccess . tcStr $ "main: (Bit -o T) -o (Bit >< QBit) -o (T >< QBit) main = \\f: Bit -o T . \\ qs: Bit >< QBit. let (x,y) = qs in (f x, y) "

-- | Simple higher order function test. Should succeed.
prop_simpleHoF = expectSuccess . tcStr $  "main: (Bit -o Bit) -o !Bit -o Bit main f x = f x"

-- | Test that new can receive unlinear bit. Should succeed.
prop_simpleApp = expectSuccess . tcStr $ "b : QBit b = new 0"

-- | Test mixed tuple type is valid as long as equal duplicity. Should succeed.
prop_funInTup = expectSuccess . tcStr $ "main : !(Bit -o QBit) >< Bit " ++  "main = (\\x : !(Bit -o QBit).(x, 0)) new"

-- | Test that a bit can be demoted.
prop_demoteBitTup = expectSuccess . tcStr $ "main : !Bit >< Bit " ++  "main = (0,0)"

-- | Test general if statement. Should succeed.
prop_ifGeneral = expectSuccess . tcStr $ "main : Bit -o Bit main x = if x then 0 else 1"

-- | Should succeed:
prop_dupIf = expectSuccess . tcStr $ "main: !Bit -o (!Bit >< Bit) main x = if x then (x,x) else (x,x)"

-- | Normal if statement.
prop_ifSig = expectSuccess . tcStr $ "f : !Bit -o !Bit " 
                                  ++ "f x = if x then 0 else 1"

-- | Test that linear and duplicable terms can be mixed in product.
prop_eqDup = expectSuccess . tcStr $ "f : Bit -o (Bit >< !Bit) f x = (x,0)"

-- | Test that let picks out correct element.
prop_letFunConst = expectSuccess . tcStr $ "main : T main = let (a,b) = (0,*) in (b)" --inferExp "\\x.\\y.let (a,b) = (x,y) in a" === Right (TypeFlex 5 (TypeFlex 3 (TypeVar "a") :=> TypeFlex 4 (TypeFlex 3 (TypeVar "b") :=> TypeFlex 3 (TypeVar "a"))))

-- | Test that let picks out correct element in big let.
prop_letFunConstBig = expectSuccess . tcStr $ "main : !Bit >< T main = let (a,b,c) = (0,new,*) in (a,c)" --inferExp "\\x.\\y.let (a,b) = (x,y) in a" === Right (TypeFlex 5 (TypeFlex 3 (TypeVar "a") :=> TypeFlex 4 (TypeFlex 3 (TypeVar "b") :=> TypeFlex 3 (TypeVar "a"))))

-- -- | Tests nested let statements with general types. should succeed.
prop_nestLet = expectSuccess . tcStr $ "main: Bit >< Bit >< Bit >< T main = let(a,b) = (0,0,0,*) in let (b,c) = b in (a,b,c)"

-- | Tests that simple let-statements infers correct type. Should succeed.
prop_leftLet = expectSuccess . tcStr $ "main: T main = let (a,b) = (*, 0) in a"
prop_rightLet = expectSuccess . tcStr $ "main: !Bit main = let (a,b) = (*, 0) in b"
prop_constLet = expectSuccess . tcStr $ "main: !Bit main = let (a,b) = (0, 1) in a"

-- | Test that double bang tuple move bang out.
prop_dupBit = expectSuccess . tcStr $ "main: !(Bit >< Bit) main = (0, 0)"

prop_linOuterIf = expectSuccess . tcStr $ "f : Bit f = 0"
                                        ++ "g : Bit g = if 1 then f else f"

-- | Test that the duplicity can be mixed.
prop_linDupExp = expectSuccess . tcStr $ "main : Bit -o !Bit >< QBit main x = (0, new 0)"

-- | Test double qubit tuple.
prop_doubleQubit= expectSuccess . tcStr $ "main : (QBit >< QBit) main = (new 0, new 0)"

-- | Should succeed.
prop_qbitFunc = expectSuccess . tcStr $ "main: !(QBit -o !Bit -o QBit) main = \\q : QBit . \\x : !Bit . q"

-- | Test that a linear qbit can't be duplicated in a closure.
prop_bangTermDup = expectSuccess . tcStr $ "q : !Bit q = 0 f : !(T -o Bit) f x = q"

-- | Test that a final dup function in the return type is fine, that y has type Bit.
prop_finalFuncBang = expectSuccess . tcStr $ "f : Bit -o !(Bit -o QBit) f x y = new y"

-- | Test that an argument can be a function type.
prop_middleFuncOk = expectSuccess . tcStr $ "f : Bit -o !(Bit -o Bit) -o Bit f x y = y x"

-- | Test eta reduce works.
prop_etaReduceOk = expectSuccess . tcStr $ "f : Bit -o QBit f = new main : Bit -o !Bit -o QBit main x = f"

-- | f is a function that should be able to take both !Bit and Bit, since !Bit <: Bit.
-- should succeed
subtypeProblem = expectSuccess . tcStr $ "a : Bit a = 0 " 
                                ++ "b : !Bit b = 0 "
                                ++ "f : !(Bit -o QBit) f = new "
                                ++ "test : (QBit >< QBit) test = (\\x:!(Bit -o QBit) . (x a, x b)) f"

-- --------------- Tests that should fail -------------------------------

-- | Test that a linear qbit can't be duplicated in a closure.
prop_linTermDup = expectError . tcStr $ "q : QBit q = new 0 f : !(T -o QBit) f x = q"


-- -- | Test that a linear function can't be used twice. Should fail.
prop_useUnlinFun = expectError . tcStr $ "f : Bit -o Bit f a = a " ++
                                         "b2 : Bit b2 = f 1 " ++ 
                                         "b3 : Bit b3 = f 1"

-- | Test that the output of a linear function cannot be made duplicable ??
prop_linFunSub = expectError . tcStr $ "f : Bit -o Bit " ++
                                       "f x = meas (new x) " ++
                                       "g : !Bit " ++
                                       "g = f 0 "

-- -- | (\x. let(a,b) = x in a) 0 should fail, since 0 is not a product type
-- -- Error, SubtypeFailError
prop_letNoProd = inferExp "(\\x : Bit . let (a,b) = x in a) 0"  === infErr (NotProduct "Bit")

-- -- | "(\\x . if new 0 then x else x) 0" should fail, since new 0 is a qubit
prop_ifNoBit = inferExp "(\\x:Bit . if new 0 then x else x) 0" === infErr (Mismatch "Bit" "QBit")

-- -- | Test that if statements with mismatching types in the then else statements throws an error. 
-- prop_MisMatchingIf = expectErrorWith (UnificationFailError TypeQBit TypeBit) (typecheck $ run "q : QBit q = new 0 f : QBit f = if 1 then q else 0") 
-- -- konstigt att vi får en linjär bit 

-- -- | Test that a linear bit cannot be used in a nonlinear function. Should fail.
-- prop_LinBit = expectErrorWith (TopLevelLinearFail "b") (typecheck . run $ "b : Bit b = 0 " 
--                                                                           ++ "dup : !(Bit >< Bit) dup = (b,b)") 

-- -- | Test that the output of a linear function cannot be made duplicable. Should fail.
-- prop_testLinFun = expectError . typecheck . run $ "f : Bit -o Bit " ++
--                                                   "f x = meas (new x) " ++
--                                                   "g : !Bit " ++
--                                                   "g = f 0 "

-- -- | Test linearity of single variable, should not be possible to have two references to a linear bit. Should fail.
-- prop_LinRef = expectError . typecheck . run $ "b1 : Bit b1 = 0 " ++
--                                               "b2 : Bit b2 = b1 " ++ 
--                                               "b3 : Bit b3 = b1"

-- -- | Test a linear function can't be used twice. Should fail.
-- prop_UseLinFunTwice = expectError . typecheck . run $ "f : Bit -o Bit f a = 0 " ++
--                                                       "b2 : Bit b2 = f 1 " ++ 
--                                                       "b3 : Bit b3 = f 1"

-- -- | Test that an argument cant be used twice in a linear function. Should fail (Subtype error) -- should it be subtype error? or something more informative?
-- prop_test = expectError $ typecheck . run $ "f : Bit -o Bit f g = if g then g else 1" 


-- -- | Passing a linear bit to something that expects an unlinear bit. Should fail (Subtype error)
-- prop_linBitAsUnlinBit = expectError . typecheck . run $ "a : Bit a = 0" ++
--                                      "f : !Bit -o !Bit f b = if b then 0 else 0" ++
--                                      "v : !Bit v = f a"
-- -- | Trying to duplicate a linear tuple, should fail.
-- prop_DupLin = expectError . typecheck . run $ "clone : (Bit >< Bit) -o ((Bit >< Bit) >< (Bit >< Bit)) " ++
--                                               "clone a = (a,a)"


-- -- | Can't give QBit to !QBit -o !QBit. Should fail.
-- prop_DupFunLinArg = expectError . typecheck . run $ "q : QBit q = new 0 " ++
--                                                     "f : !QBit -o !QBit f x = x " ++
--                                                     "main : !QBit main = f q"

-- | Test that a linear bit cannot be used in a non-linear way in an if statement. Should fail.
-- prop_IfUnLin = tcStr "f : Bit -o Bit f g = if g then g else 1" 

-- -- | Test that it is not possible to create a duplicable qubit. Should fail.
-- prop_DupQbit = expectError . typecheck . run $  "q : !QBit q = new 0"

-- -- | Test that mismatched duplicity in a product type gives an error. Should fail.
-- prop_linDupExp = expectSuccess $ inferExp "(0, new 0)"


-- prop_dupQbit = expectError . typecheck . run $ "f : !QBit -o !(QBit >< QBit) "
--                                             ++ "f x = (x,x) "
--                                             ++ "g : !(QBit >< QBit) "
--                                             ++ "g = f (new 0)"

-- -------------- Tests that are not working  -----------------

-- prod =  inferExp "(0, \\x.x)"

-- -- | Test that the inferred expression have the correct type.  should have type. !a -o !b -o !(a, a) and succeed.
-- -- ?(!a -o !b -o !(a >< a))
-- -- inferred type: ?(?a -o ?(!b -o !(a >< a)))
-- -- Right !a ⊸ !b ⊸ !(a ⊗ a)
-- -- prop_letFunDup = inferExp "\\x.\\y.let (a,b) = (x,y) in (a,a)"  === Right "!a -o !b -o !(a >< a)" -- Right (TypeDup (TypeVar "a") :=>  TypeDup (TypeVar "b") :=> TypeDup (TypeVar "a" :>< TypeVar "a"))
-- prop_letFunDup = expectSuccess $ inferExp "\\x.\\y.let (a,b) = (x,y) in (a,a)"  

-- plfd = typecheck . run $ "f : !a -o !b -o !(a >< a) "
--                       ++ "f x y = let (a,b) = (x,y) in (a,a)"

-- prop_good = inferExp "(\\x.\\y.(x,y)) 0 (new 0)" === Right "Bit >< QBit"
-- prop_bad = inferExp "(\\x.\\y.(x,y)) (new 0) 0" === Right "QBit >< Bit"
-- prop_ugly = inferExp "(\\x.\\y.let (a,b) = (x,y) in a) (new 0) 0" === Right TypeQBit

-- reallyBad = typecheck . run $ "f : (a >< b) -o !b f x = let (a,b) = x in a"

-- Need return for quickCheckAll

-- -- ?{3}(?{2}(a ⊗  b) ⊸ ?{2}(b)) 

-- -- typecheck . run $ "f : (a >< b) -o !b f x = let (a,b) = x in a"

-- -- should work, trixa liet med ifsatsen
-- f = typecheck . run $ "f : Bit f = 0 g : Bit g = if 1 then f else f"



-- -- simpler example with same problem
-- stProb = inferExp "(\\x . (x 0, x 0)) new"

-- even simpler example
-- stp = inferExp "\\x.(x 0, x 0)"

-- tes = inferExp "(\\x . x 0) new"

-- -- not working, problem when we say that x must be duplicable, should work?
-- stProb2 = inferExp "(\\x . (new x, new x)) 0"

return []

-- Run all tests 
runTests :: IO Bool
runTests = $quickCheckAll

-- --- Test that should fail (throw an exception)
-- expectErrorWith :: TypeError -> Either TypeError t -> Property
-- expectErrorWith err res = case res of 
--     Left err' -> property (err == err')
--     Right _   -> property False  


-- expectSuccess :: Either e t -> Property 
-- expectSuccess = property . isRight

-- -------------- Tests that should succeed ------------------------------
