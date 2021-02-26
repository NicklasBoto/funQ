-- | Quantum teleportation example
module Teleport where

import FunQ

-- Example usage:
--
-- >>> dist $ measure =<< teleport =<< hadamard =<< new 0
-- Runs : 100
-- |0>  : 46.0 (46.0 %)
-- |1>  : 54.0 (54.0 %)
--
-- >>> dist exampleTeleport
-- Runs : 100
-- |0>  : 100.0 (100.0 %)
-- |1>  : 0.0 (0.0 %)

-- | The quantum teleportation algorithm
-- 1. create EPR pair (a,b)
-- 2. perform bell measurment on (psi, a)
-- 3. perform corrections on b according to the bell measurement
teleport :: QBit -> QM QBit
teleport psi = do
    a <- new 0
    b <- new 0
    hadamard a
    cnot (a,b)
    cnot (psi,a)
    hadamard psi
    m_psi <- measure psi
    m_a <- measure a
    pauliX b `controlbit` m_a
    pauliZ b `controlbit` m_psi
    return b

-- | Create a qubit, teleport it, and measure it
exampleTeleport :: QM Bit
exampleTeleport = do
    q <- new 0
    hadamard q
    -- perform manipulations here
    q' <- teleport q
    -- the resulting distribution should be the same as
    -- for @q@ before the teleportation
    measure q'

correction :: QBit -> (Bit, Bit) -> QM QBit
correction q (x,y) = do
    pauliX q `controlbit` y
    pauliZ q `controlbit` x
    return q

teleport' :: QBit -> QM QBit
teleport' psi = do
    (a,b) <- bell (0,0)
    m <- bellMeasure (psi,a)
    correction b m
