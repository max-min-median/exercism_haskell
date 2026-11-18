module Roman (numerals) where

numerals :: Integer -> Maybe String
numerals n
  | n < 1 || n > 3999 = Nothing
  | otherwise         = Just (go n [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"),
                                    (50,"L"), (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I") ])
  where
    go 0 _        = ""
    go _ []       = error "Unreachable"
    go x yss@((v,s):ys)
      | x >= v    = s <> go (x-v) yss
      | otherwise = go x ys