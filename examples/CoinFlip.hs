module CoinFlip where


import FunQ

-- | Given two values, will return either of them with a 50% probability.

-- Example usage:
--   :l examples/CoinFlip.hs -- Load the haskell file
--   run $ coinFlip "hard brackets" "parenthesis"

coinFlip :: a -> b -> QM (Either a b)
coinFlip left right = do
    q <- new 0
    q <- hadamard q
    b <- measure q
    return $ if b == 0 then Left left else Right right
