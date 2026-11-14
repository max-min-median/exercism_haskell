module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Control.Concurrent.STM

data BankAccount = BankAccount (TVar Integer) (TVar Bool)

closeAccount :: BankAccount -> IO ()
closeAccount (BankAccount _ open) = atomically $ writeTVar open False

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance (BankAccount bal open) = atomically $ do
  o <- readTVar open
  if o then
    readTVar bal >>= pure . Just
  else
    pure Nothing

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance (BankAccount bal open) amount = atomically $ do
  o <- readTVar open
  if o then do
    modifyTVar' bal (+amount)
    readTVar bal >>= pure . Just
  else
    pure Nothing

openAccount :: IO BankAccount
openAccount = BankAccount <$> newTVarIO 0 <*> newTVarIO True
