module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise         = Just (go n)
  where
    go x
      | x >= 1000 = 'M' : go (x-1000)
      | x >= 900  = 'C' : 'M' : go (x-900)
      | x >= 500  = 'D' : go (x-500)
      | x >= 400  = 'C' : 'D' : go (x-400)
      | x >= 100  = 'C' : go (x-100)
      | x >= 90   = 'X' : 'C' : go (x-90)
      | x >= 50   = 'L' : go (x-50)
      | x >= 40   = 'X' : 'L' : go (x-40)
      | x >= 10   = 'X' : go (x-10)
      | x >= 9    = 'I' : 'X' : go (x-9)
      | x >= 5    = 'V' : go (x-5)
      | x >= 4    = 'I' : 'V' : go (x-4)
      | x >= 1    = 'I' : go (x-1)
      | otherwise = ""


