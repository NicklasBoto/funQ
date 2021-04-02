import Type.HM
import AST.AST

-- | Inferring 0 should be !Bit
testBit = inferExp "0" == Right (TypeDup TypeBit)

-- | (\x. let(a,b) = x in a) 0 should fail, since 0 is not a product type
testLetNoProd = inferExp "(\\x.let (a,b) = x in a) 0"

-- | The inside is (!Bit, !Bit) which makes ! move outside, to !(Bit, Bit)
testDupProd = inferExp "(0, 0)" == Right (TypeDup (TypeBit :>< TypeBit))

-- | Expected (?a, ?b) -o ?a
testFst = inferExp "\\x.let (a,b) = x in a" == Right ((TypeVar "a" :>< TypeVar "b") :=> TypeVar "a")

-- | \x.x should have type ?Bit->?Bit, since it could be either linear or not linear.
testId = inferExp "\\x.x" == Right (TypeVar "a" :=> TypeVar "a")

