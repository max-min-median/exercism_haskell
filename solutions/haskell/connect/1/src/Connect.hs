{-# LANGUAGE LambdaCase #-}

module Connect (Mark(..), winner) where

import Data.Array
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner []           = Nothing
winner ["X"]        = Just Cross
winner ["O"]        = Just Nought
winner board'@(x:_) = if areLRconnected rows cols uf then Just Cross
                       else if areUDconnected rows cols uf then Just Nought
                       else Nothing
  where
    board'' = map words board'
    (rows,cols) = (length board'', length (words x))
    board = listArray ((0,0),(rows-1,cols-1)) . concatMap words $ board' :: Array (Int,Int) String
    uf = foldr step empty (assocs board)
      where
        step :: ((Int,Int),String) -> UF (Int,Int) -> UF (Int,Int)
        step ((r,c),s) uf'
          | s == "."  = uf'
          | otherwise = let unionLeft    = if c > 0 && board!(r,c-1) == s then unionOrAdd (r,c) (r,c-1) else id
                            unionUpLeft  = if r > 0 && board!(r-1,c) == s then unionOrAdd (r,c) (r-1,c) else id
                            unionUpRight = if r > 0 && c < cols-1 && board!(r-1,c+1) == s then unionOrAdd (r,c) (r-1,c+1) else id
                        in  unionLeft . unionUpLeft . unionUpRight $ uf'

areLRconnected :: Int -> Int -> UF (Int,Int) -> Bool
areLRconnected rows cols uf = fromMaybe False (fst <$> connected (0,-1) (0,-2) uf')
  where
    uf' = unionSide (0,-1) [(r,0) | r <- [0..rows-1]] . unionSide (0,-2) [(r,cols-1) | r <- [0..rows-1]] $ uf

areUDconnected :: Int -> Int -> UF (Int,Int) -> Bool
areUDconnected rows cols uf = fromMaybe False (fst <$> connected (-1,0) (-2,0) uf')
  where
    uf' = unionSide (-1,0) [(0,c) | c <- [0..cols-1]] . unionSide (-2,0) [(rows-1,c) | c <- [0..cols-1]] $ uf

unionSide :: (Int, Int) -> [(Int, Int)] -> UF (Int, Int) -> UF (Int, Int)
unionSide v vs uf = foldr (unionOrAdd v) uf vs

------------------- my Union-Find module -------------------

data UF a = UF !(M.Map a a) !(M.Map a Int) !Int

empty :: UF a
empty = UF M.empty M.empty 0

addVertex :: Ord a => a -> UF a -> UF a
addVertex v uf@(UF parents ranks ct)
  | v `M.member` parents = uf
  | otherwise            = UF (M.insert v v parents) (M.insert v 0 ranks) (ct+1)

find :: Ord a => a -> UF a -> Maybe (a, UF a)
find v uf@(UF parents ranks ct) = do
  vp <- M.lookup v parents
  if v == vp then
    pure (v, uf)
  else do
    (vRoot, UF parents' _ _) <- find vp uf
    pure (vRoot, UF (M.insert v vRoot parents') ranks ct)

connected :: Ord a => a -> a -> UF a -> Maybe (Bool, UF a)
connected u v uf = do
  (up, uf') <- find u uf
  (vp, uf'') <- find v uf'
  pure (up == vp, uf'')

union :: Ord a => a -> a -> UF a -> Maybe (UF a)
union u v uf = do
  (up, uf') <- find u uf
  (vp, uf'') <- find v uf'
  if up == vp then
    pure uf''
  else
    let UF parents ranks ct = uf''
        ur = ranks M.! up
        vr = ranks M.! vp
    in case compare ur vr of
      LT -> pure $ UF (M.insert up vp parents) ranks (ct-1)
      GT -> pure $ UF (M.insert vp up parents) ranks (ct-1)
      EQ -> pure $ UF (M.insert vp up parents) (M.insert up (ur+1) ranks) (ct-1)

unionOrAdd :: Ord a => a -> a -> UF a -> UF a
unionOrAdd u v uf = case union u v . addVertex u . addVertex v $ uf of
  Just uf' -> uf'
  Nothing  -> error "Unreachable"
