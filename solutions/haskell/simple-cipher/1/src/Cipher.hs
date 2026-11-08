module Cipher (caesarDecode, caesarEncode, caesarEncodeRandom) where

import System.Random
import System.Random.Stateful
import Control.Monad (replicateM)

caesarDecode :: String -> String -> String
caesarDecode = genericCaesar negate

caesarEncode :: String -> String -> String
caesarEncode = genericCaesar id

genericCaesar :: (Int -> Int) -> String -> String -> String
genericCaesar fn key text = encoded
  where
    offsets = cycle . map (fn . (subtract (fromEnum 'a')). fromEnum) $ key
    ordinals = map ((subtract (fromEnum 'a')). fromEnum) text
    rotated = zipWith (+) ordinals offsets
    encoded = map (\n -> toEnum (fromEnum 'a' + n `mod` 26)) rotated

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom text = do
  len <- uniformRM (5,30) globalStdGen :: IO Int
  key <- (map (toEnum . (+ fromEnum 'a'))) <$> replicateM len (uniformRM (5,30) globalStdGen :: IO Int) :: IO String
  pure (key, caesarEncode key text)