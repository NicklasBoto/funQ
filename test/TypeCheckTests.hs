-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE LambdaCase      #-}
-- {-# LANGUAGE OverloadedStrings      #-}

module TypeCheckTests where 
import Type.TypeChecker

f = tcStr "f:!(Bit -o Bit) f=(\\a(Bit).0)"
a = tcStr "f:!(T -o QBit) f = \\x(T). new 0"
-- import Type.HM
-- import AST.AST
-- import Data.Either (lefts,rights, isLeft, isRight)
-- import Test.QuickCheck (quickCheckAll, (===), Property, Testable(property), quickCheck)

-- --- Test that should fail (throw an exception)
-- expectError :: Either e t -> Property
-- expectError = property . isLeft

-- --- Test that should fail (throw an exception)
-- expectErrorWith :: TypeError -> Either TypeError t -> Property
-- expectErrorWith err res = case res of 
--     Left err' -> property (err == err')
--     Right _   -> property False  


-- expectSuccess :: Either e t -> Property 
-- expectSuccess = property . isRight

-- -------------- Tests that should succeed ------------------------------

-- -- | Inferring 0 should be !Bit
-- prop_testBit = inferExp "0" === Right (TypeFlex 0 TypeBit)
-- -- | The inside is (!Bit, !Bit) which makes ! move outside, to !(Bit, Bit)
-- prop_testDupProd = inferExp "(0, 0)" === Right (TypeFlex 1 (TypeBit :>< TypeBit))

-- -- | Expected (?a, ?b) -o ?a
-- prop_testFst = inferExp "\\x.let (a,b) = x in a" === Right (TypeFlex 3 (TypeFlex 2 (TypeVar "a" :>< TypeVar "b") :=> TypeFlex 2 (TypeVar "a")))
-- -- == Right ((TypeFlex "a" :>< TypeFlex "b") :=> TypeFlex "a")

-- -- | \x.x should have type ?a->?a, since it could be either linear or not linear.
-- prop_id = inferExp "\\x.x" === Right (TypeFlex 1 (TypeFlex 0 "a" :=> TypeFlex 0 "a"))

-- -- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
-- prop_IfLinBit = expectSuccess . typecheck . run $ "f : Bit -o Bit f g = if g then 0 else 1"

-- -- | Test that a linear bit can be used as a condition in an if statement. Should succeed.
-- -- fails unsuccessfully
-- prop_IfLinBit2 = expectSuccess . typecheck . run $ "g : Bit g = 0 " ++ 
--                                     "f : Bit f = if g then 0 else 1"

-- -- | Test that an unlinear bit can be used as a condition in an if statement. Should succeed.
-- prop_IfNormalBit = expectSuccess . typecheck . run $ "g : !Bit g = 0 " ++ 
--                                      "f : Bit f = if g then 0 else 1"

-- -- | Test a nonlinear function can be used many times. Should succeed.
-- prop_UseLinFun = expectSuccess . typecheck . run $ "f : !(Bit -o Bit) f a = 0 " ++
--                                                    "b2 : Bit b2 = f 1 " ++ 
--                                                    "b3 : Bit b3 = f 1"

-- -- | Can assign a unlinear bit to a linear bit. Should succeed.
-- prop_UnLinBit = expectSuccess . typecheck . run $ "f : Bit f = 0"

-- -- | Higher order functions test. Should succeed.
-- prop_SubtypeHoF = expectSuccess $ inferExp "\\f . \\ qs . let (x,y) = qs in (f x, y)" -- == Right ((TypeFlex "a" :=> TypeFlex "b"):=> (TypeFlex "a" :><  TypeFlex "c") :=> TypeFlex "b" :>< TypeFlex "c")

-- -- | Simple higher order function test. Should succeed.
-- prop_SimpleHoF = expectSuccess $ inferExp "\\f . \\x . f x" -- == Right ((TypeFlex "a" :=> TypeFlex "b") :=> TypeFlex "a" :=> TypeFlex "b")

-- -- | Test that new can receive unlinear bit. Should succeed.
-- prop_SimpleApp = expectSuccess . typecheck . run $ "b : QBit b = new 0"

-- -- | Test mixed tuple type is valid as long as equal duplicity. Should succeed.
-- prop_FunInTup = expectSuccess $ inferExp "(\\x.(x,x,x,x,0,1)) new"

-- -- | Test general if statement. Should succeed.
-- prop_IfGeneral = expectSuccess $ inferExp "\\x. if x then 0 else 1"

-- -- | Should succeed: Right !Bit ⊸ !(Bit ⊗ Bit)
-- prop_DupIf = expectSuccess $ inferExp "\\x.if x then (x,x) else (x,x)"

-- -- | 
-- prop_IfSig = expectSuccess $ typecheck . run $ "f : !Bit -o !Bit " 
--                                             ++ "f x = if x then 0 else 1"

-- -- | Test that we can infer a general type for x. Should succeed. 
-- prop_EqDup = expectSuccess $ inferExp "\\x.(x,0)"


-- -- | Test that x and y get same flex id and that it type checks. Should succeed
-- prop_letFunConst = inferExp "\\x.\\y.let (a,b) = (x,y) in a" === Right (TypeFlex 5 (TypeFlex 3 (TypeVar "a") :=> TypeFlex 4 (TypeFlex 3 (TypeVar "b") :=> TypeFlex 3 (TypeVar "a"))))



-- -- | Tests nested let statements with general types. should succeed.
-- prop_NestLet = inferExp "\\x . let (a,b) = x in let (b,c) = b in (a,b,c)" ===  Right (TypeFlex 5 (TypeFlex 4 (TypeVar "a" :>< TypeVar "b" :>< TypeVar "c") :=> TypeFlex 4 (TypeVar "a" :>< TypeVar "b" :>< TypeVar "c")))

-- -- | Tests that simple let-statements infers correct type. Should succeed.
-- prop_LeftLet = inferExp "let (a,b) = (*, 0) in a" === Right (TypeDup TypeUnit)
-- prop_RightLet = inferExp "let (a,b) = (*, 0) in b" === Right (TypeDup TypeBit)
-- prop_ConstLet = inferExp "let (a, b) = (0, 1) in a" === Right (TypeFlex 1 TypeBit)


-- -- | Test that the inferred type of a simple bit tuple is correct. Should succeed.
-- prop_dupBit = inferExp "(0, 0)" === Right (TypeFlex 1 (TypeBit :>< TypeBit))

-- -- | Test that a general function gets a flexible type. Should succeed.
-- prop_DupFlexExp = inferExp "\\x. \\y. (x,y)" === Right (TypeFlex 3 (TypeFlex 1 a :=> TypeFlex 2 (TypeFlex 1 b :=> TypeFlex 1 (a :>< b))))
--     where
--         a = TypeVar "a"
--         b = TypeVar "b"

-- -- | Test that the duplicity is the same for all elements in a tuple. Should succeed.
-- prop_DupExp = inferExp "\\x.(x,0)" === Right (TypeFlex 2 (TypeFlex 1 a :=> TypeFlex 1 (a :>< TypeBit)))
--     where
--         a = TypeVar "a"

-- -- | Test that the duplicity is the same for all elements in a tuple. Should succeed.
-- prop_LinExp = inferExp "\\x.(x,new 0)" === Right (TypeFlex 3 (a :=> a :>< TypeQBit))
--     where
--         a = TypeVar "a"

-- -- | Test that the duplicity is the same for all elements in a tuple. Should succeed.
-- prop_LinExp2 = inferExp "\\x.(new 0, x)" === Right (TypeFlex 3 (a :=> TypeQBit :>< a))
--     where
--         a = TypeVar "a"
        
-- -- | Test that the duplicity is the same for all elements in a tuple. Should succeed.
-- prop_linLinExp = inferExp "(new 0, new 0)" === Right (TypeQBit :>< TypeQBit)


-- --------------- Tests that should fail -------------------------------

-- -- | Test that a linear function can't be used twice. Should fail.
-- prop_UseUnlinFun = expectError . typecheck . run $ "f : Bit -o Bit f a = a " ++
--                                                    "b2 : Bit b2 = f 1 " ++ 
--                                                    "b3 : Bit b3 = f 1"

-- -- | Test that the output of a linear function cannot be made duplicable ??
-- prop_linFunSub = expectError . typecheck . run $ "f : Bit -o Bit " ++
--                                                  "f x = meas (new x) " ++
--                                                  "g : !Bit " ++
--                                                  "g = f 0 "

-- -- | (\x. let(a,b) = x in a) 0 should fail, since 0 is not a product type
-- -- Error, SubtypeFailError
-- prop_LetNoProd = expectError $ inferExp "(\\x.let (a,b) = x in a) 0" -- === Left (SubtypeFailError (TypeVar "c" :>< TypeVar "d") (TypeDup TypeBit))

-- -- | "(\\x . if new 0 then x else x) 0" should fail, since new 0 is a qubit
-- prop_IfNoBit = expectError $ inferExp "(\\x . if new 0 then x else x) 0"

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

-- -- | Test that a linear bit cannot be used in a non-linear way in an if statement. Should fail.
-- prop_IfUnLin = expectError . typecheck . run $ "f : Bit -o Bit f g = if g then g else 1"

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

-- -- Need return for quickCheckAll
-- return []

-- -- Run all tests 
-- runTests :: IO Bool
-- runTests = $quickCheckAll

-- -- ?{3}(?{2}(a ⊗  b) ⊸ ?{2}(b)) 

-- -- typecheck . run $ "f : (a >< b) -o !b f x = let (a,b) = x in a"

-- -- should work, trixa liet med ifsatsen
-- f = typecheck . run $ "f : Bit f = 0 g : Bit g = if 1 then f else f"

-- -- f is a function that should be able to take both !Bit and Bit, since !Bit <: Bit.
-- -- should succeed
-- subtypeProblem = typecheck . run $ "a : Bit a = 0 " 
--                                 ++ "b : !Bit b = 0 "
--                                 ++ "f : !(Bit -o QBit) f = new "
--                                 ++ "test : Bit test = (\\x . (x a, x b)) f"


-- -- simpler example with same problem
-- stProb = inferExp "(\\x . (x 0, x 0)) new"

-- tes = inferExp "(\\x . x 0) new"

-- -- not working, problem when we say that x must be duplicable, should work?
-- stProb2 = inferExp "(\\x . (new x, new x)) 0"