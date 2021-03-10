module Arbitraries ( QState(..) ) where 

import Test.QuickCheck
import QM ( QState(..) )
import Internal.Core (newVector, tensorVector)

-- Arbitrary instance for QState, not very pretty :) 
instance Arbitrary QState where
    arbitrary = do
        b <- elements [0,1]
        n <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        m <- elements [state $ newVector b, tensorVector (state $ newVector b) (state $ newVector b)]
        let s = elements [tensorVector m n]
        let t = elements [tensorVector (tensorVector m n) (tensorVector m n)]
        e <- frequency [(7,s),(3,t)]
        return $ QState e

-- 
-- 
-- -- Not used for now
-- instance Arbitrary (QM QBit) where
--     arbitrary = elements [new 0, new 1]
-- 
-- -- Not used for now
-- instance Arbitrary (QM QState) where
--         arbitrary = do
--             put <$> (arbitrary :: Gen QState)
--             return get
-- 
-- -- Not used for now
-- instance Arbitrary (QM ()) where
--   arbitrary = put <$> (arbitrary :: Gen QState)