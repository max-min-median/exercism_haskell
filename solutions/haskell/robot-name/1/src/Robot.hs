module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad.State
import Data.IORef
import qualified Data.Set as S
import System.Random.Stateful
import Control.Monad (replicateM)

newtype Robot = Robot {getName :: IORef String}
data RunState = RunState {robotNames :: S.Set String}

initialState :: RunState
initialState = RunState S.empty

mkRobot :: StateT RunState IO Robot
mkRobot = do
  currNames <- gets robotNames
  newName <- liftIO $ randName currNames
  put $ RunState (S.insert newName currNames)
  liftIO $ Robot <$> newIORef newName


resetName :: Robot -> StateT RunState IO ()
resetName robot = do
  name <- liftIO $ readIORef (getName robot)
  currNames <- gets robotNames
  newName <- liftIO $ randName currNames
  liftIO $ writeIORef (getName robot) newName
  put $ RunState (S.delete name (S.insert newName currNames))

randName :: S.Set String -> IO String
randName blacklist = do
  name <- replicateM 2 randChar <> replicateM 3 randDigit
  if name `S.member` blacklist then randName blacklist else pure name
  where
    randDigit = uniformRM ('0','9') globalStdGen
    randChar = uniformRM ('A','Z') globalStdGen

robotName :: Robot -> IO String
robotName = readIORef . getName
