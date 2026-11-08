{-# LANGUAGE LambdaCase #-}

module Deque (Deque, mkDeque, pop, push, shift, unshift) where

import Data.IORef

data Node a = Sentinel | Node (IORef (Node a)) a (IORef (Node a))
data Deque a = Deque (IORef (Node a)) (IORef (Node a))

mkDeque :: IO (Deque a)
mkDeque = Deque <$> newIORef Sentinel <*> newIORef Sentinel

pop :: Deque a -> IO (Maybe a)
pop (Deque hRef tRef) = readIORef tRef >>= \case
  Sentinel           -> pure Nothing
  Node prevRef val _ -> readIORef prevRef >>= \case
    Sentinel                    -> writeIORef hRef Sentinel >> writeIORef tRef Sentinel >> pure (Just val)
    prev@(Node _ _ prevNextRef) -> writeIORef prevNextRef Sentinel >> writeIORef tRef prev >> pure (Just val)

push :: Deque a -> a -> IO ()
push (Deque hRef tRef) x = readIORef tRef >>= \case
  Sentinel                    -> do
    newNode <- Node <$> newIORef Sentinel <*> pure x <*> newIORef Sentinel
    writeIORef hRef newNode >> writeIORef tRef newNode
  prev@(Node _ _ prevNextRef) -> do
    newNode <- Node <$> newIORef prev <*> pure x <*> newIORef Sentinel
    writeIORef prevNextRef newNode >> writeIORef tRef newNode

unshift :: Deque a -> a -> IO ()
unshift (Deque hRef tRef) x = readIORef hRef >>= \case
  Sentinel                    -> do
    newNode <- Node <$> newIORef Sentinel <*> pure x <*> newIORef Sentinel
    writeIORef hRef newNode >> writeIORef tRef newNode
  next@(Node nextPrevRef _ _) -> do
    newNode <- Node <$> newIORef Sentinel <*> pure x <*> newIORef next
    writeIORef nextPrevRef newNode >> writeIORef hRef newNode

shift :: Deque a -> IO (Maybe a)
shift (Deque hRef tRef) = readIORef hRef >>= \case
  Sentinel           -> pure Nothing
  Node _ val nextRef -> readIORef nextRef >>= \case
    Sentinel                    -> writeIORef hRef Sentinel >> writeIORef tRef Sentinel >> pure (Just val)
    next@(Node nextPrevRef _ _) -> writeIORef nextPrevRef Sentinel >> writeIORef hRef next >> pure (Just val)
