module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go [] (filter (`elem` "()[]{}") xs)
  where
    go [] "" = True
    go _ ""  = False
    go stk (x:xs)
      | x `elem` "([{" = go (closer x : stk) xs
    go (a:as) (x:xs)
      | x == a = go as xs
    go _ _ = False

closer :: Char -> Char
closer '(' = ')'
closer '[' = ']'
closer '{' = '}'
closer _ = error "Bad bracket"