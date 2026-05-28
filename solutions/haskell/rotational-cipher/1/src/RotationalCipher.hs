module RotationalCipher (rotate) where

import Data.Char (isAlpha)

rotate :: Int -> String -> String
rotate n = map rot
  where
    rot ch = if not (isAlpha ch) then ch else
      let ascii = fromEnum ch
          case' = ascii `div` 32 * 32
          pos   = ascii `mod` 32 - 1
      in toEnum $ case' + 1 + (pos + n) `mod` 26