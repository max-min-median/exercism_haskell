module Sieve (primesUpTo) where

-- You should not use any of the division operations when implementing
-- the sieve of Eratosthenes.
import Prelude hiding (div, mod, divMod, rem, quotRem, quot, (/))

import qualified Data.Vector.Unboxed.Mutable as MVU
import Control.Monad
import Control.Monad.ST
import Data.STRef

primesUpTo :: Integer -> [Integer]
primesUpTo n = runST st
  where
    st :: ST s [Integer]
    st = do
      vec <- MVU.replicate (fromIntegral n + 1) True
      result <- newSTRef []
      forM_ [2..n] $ \i -> do
        isPrime <- MVU.read vec (fromIntegral i)
        if isPrime then do
          modifySTRef' result (i:)
          forM_ [i,2*i..n] $ \j -> MVU.write vec (fromIntegral j) False
        else
          pure ()
      reverse <$> readSTRef result
  