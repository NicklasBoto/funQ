import Test.QuickCheck;
import QData
import QOps

main :: IO ()
main = do
    quickCheck prop_qubitEq1
    quickCheck prop_qubitEq2
    quickCheck prop_qubitEq3
    quickCheck prop_qubitEq4

-- TODO:
-- eq qubits
-- arbitrary qubit
-- prop_paulixGate :: QBit n -> Bool
-- prop_paulixGate q = undefined -- q == pauliX pauliX q

prop_qubitEq1 :: Bool
prop_qubitEq1 = new 0 /= new 1

prop_qubitEq2 :: Bool
prop_qubitEq2 = new 1 /= new 0

prop_qubitEq3 :: Bool
prop_qubitEq3 = new 1 == new 1

prop_qubitEq4 :: Bool
prop_qubitEq4 = new 0 == new 0