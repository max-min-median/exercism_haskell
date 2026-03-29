module StateOfTicTacToe (gameState, GameState(..)) where

import Data.List ( transpose )

data GameState = WinX | WinO | Draw | Ongoing | Impossible deriving (Eq, Show)
data Player = X | O deriving Eq
type Board = [String]

playerChar :: Player -> Char
playerChar X = 'X'
playerChar O = 'O'

playerWins :: Player -> Board -> Bool
playerWins p board = any (elem threeInARow) [board, transpose board, [[st !! i | (i, st) <- indexedBoard]], [[st !! (2-i) | (i, st) <- indexedBoard]]]
  where
    threeInARow = replicate 3 $ playerChar p
    indexedBoard = zip [0..] board

countSymbols :: Player -> Board -> Int
countSymbols p = length . concatMap (filter (== playerChar p))

gameState :: [String] -> GameState
gameState board
  | xWins && not oWins && nX == nO + 1 = WinX
  | oWins && not xWins && nO == nX = WinO
  | xWins && oWins || nX > nO + 1 || nX < nO = Impossible
  | nX + nO == 9 = Draw
  | otherwise = Ongoing
  where
    xWins = playerWins X board
    oWins = playerWins O board
    nO = countSymbols O board
    nX = countSymbols X board

