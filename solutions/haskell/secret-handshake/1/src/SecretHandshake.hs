module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = (if testBit n 4 then reverse else id) [act | (i, act) <- zip [0..3] ["wink", "double blink", "close your eyes", "jump"], testBit n i]