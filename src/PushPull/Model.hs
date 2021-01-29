module PushPull.Model
  ( Push
  , Pull
  , pushIn
  , pushOut
  , pullIn
  , pullOut
  , read
  , write
  , enrich
  , latest
  , previous
  , all
  , PushPull.Model.fail
  , failure
  , module Data.Void
  , module Control.Exception.Base
  , module Control.Concurrent.STM
  , module Control.Category
  , module Control.Arrow
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible
  ) where

import Prelude hiding (read, (.), id, all)

import Data.Semigroupoid
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Data.Profunctor
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Exception.Base
import Control.Category
import Control.Monad
import Control.Arrow
import PushPull.STMExtras

-- Push

newtype Push ctx a = Push (ctx -> a -> STM ())

instance Contravariant (Push ctx) where
  contramap f (Push p) = Push $ \c -> p c . f

instance Divisible (Push ctx) where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in p1 c b1 >> p2 c b2
  conquer = Push $ const $ const $ return ()

instance Decidable (Push ctx) where
  choose f (Push p1) (Push p2) = Push $ \c -> either (p1 c) (p2 c) . f
  lose f = Push $ const $ absurd . f

instance Semigroup (Push ctx a) where
  p1 <> p2 = divide (\a -> (a, a)) p1 p2

instance Monoid (Push ctx a) where
  mempty = conquer

-- Pull

newtype Pull ctx a = Pull (ctx -> STM a)

instance Category Pull where
  id = Pull return
  Pull pull2 . Pull pull1 = Pull $ pull1 >=> pull2

instance Profunctor Pull where
  rmap f (Pull pull) = Pull $ fmap f <$> pull
  lmap f (Pull pull) = Pull $ pull . f

instance Arrow Pull where
  arr f = Pull $ return . f
  first (Pull pull) = Pull $ \(c, d) -> (,) <$> pull c <*> pure d

instance Functor (Pull ctx) where
  fmap = rmap

instance Applicative (Pull ctx) where
  pure = Pull . const . return
  Pull f <*> Pull a = Pull $ \c -> f c <*> a c

instance Monad (Pull ctx) where
  return = pure
  (Pull pull1) >>= f = Pull $ \c -> do
    Pull pull2 <- f <$> pull1 c
    pull2 c

-- Cell

data Cell ctx a b = Cell {
  write :: Push ctx a,
  read :: Pull ctx b
}

instance Profunctor (Cell ctx) where
  rmap f (Cell w r) = Cell w (fmap f r)
  lmap f (Cell w r) = Cell (contramap f w) r

-- Cell is not a Category (lack of id :: Cell ctx a a) => Cell is not an Arrow either.
-- At least it's a Semigroupoid:
-- Still, not sure if it's Cell Semigroupoing is good idea: you're still able to push only to cell1 and cell2 will not be affected
instance Semigroupoid (Cell ctx) where
  Cell (Push push2) (Pull pull2) `o` Cell (Push push1) (Pull pull1)  = Cell (Push push1and2) (Pull pull2)
    where
      push1and2 c a = do
        push1 c a
        b <- pull1 c
        push2 c b
-- one might think there's an alternative implementation, but it's invalid:
-- Cell (Push push2) (Pull pull2) `o` Cell (Push push1) (Pull pull1) = Cell (Push push1) (Pull pull1and2)
--    where
--      pull1and2 c = do
--        b <- pull1 c
--        push2 c b -- this is wrong: we shoud not push each time we pull!
--        pull2 c

instance Functor (Cell ctx a) where
  fmap = rmap

instance Applicative (Cell ctx a) where
  pure a = Cell conquer (pure a)
  Cell pushg1 pullf <*> Cell pushg2 pulla = Cell (divide (\a -> (a, a)) pushg1 pushg2) (pullf <*> pulla)

instance Monad (Cell ctx a) where
  return = pure
  Cell (Push push1) (Pull pull1) >>= f = let
    push c a = do
      push1 c a
      b <- pull1 c
      let Cell (Push push2) _ = f b
      push2 c a
    pull c = do
      b <- pull1 c
      let Cell _ (Pull pull2) = f b
      pull2 c
    in Cell (Push push) (Pull pull)

latest :: IO (Cell ctx a (Maybe a))
latest = atomically $ do
  var <- newTMVar Nothing
  return $ Cell {
    write = Push $ const $ putTMVar var . Just,
    read = Pull $ const $ readTMVar var
  }

previous :: IO (Cell ctx a (Maybe a))
previous = atomically $ do
  var <- newTMVar (Nothing, Nothing)
  return $ Cell {
    write = Push $ const $ \a -> modifyTMVar var (\(latest, _) -> (Just a, latest)),
    read = Pull $ const $ snd <$> readTMVar var
  }

count :: IO (Cell ctx a Int)
count = atomically $ do
  var <- newTMVar 0
  return $ Cell {
    write = Push $ const $ \a -> modifyTMVar var (+ 1),
    read = Pull $ const $ readTMVar var
  }

all :: IO (Cell ctx a [a])
all = atomically $ do
  var <- newTMVar []
  return $ Cell {
    write = Push $ const $ modifyTMVar var . (:),
    read = Pull $ const $ readTMVar var
  }

-- Push/Pull

enrich :: Pull ctx b -> (a -> b -> c) -> Push ctx c -> Push ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> do
  b <- pull c
  push c $ f a b

-- I/O, failures

fail :: Exception e => Push ctx e
fail = Push $ const throwSTM

failure :: Exception e => e -> Pull ctx a
failure = Pull . const . throwSTM

pushOut :: Int -> (a -> IO ()) -> IO (Push ctx a)
pushOut queueSize consume = do
  q <- newTBQueueIO (fromIntegral queueSize)
  forkIO $ forever $ do
    a <- atomically $ readTBQueue q
    consume a
  return $ Push $ const $ writeTBQueue q

pullIn :: Int -> IO a -> IO (Pull ctx a)
pullIn periodMilliseconds producer = do
  a <- producer
  var <- newTMVarIO a
  forkIO $ forever $ do
    threadDelay $ periodMilliseconds * 1000
    a <- producer
    atomically $ putTMVar var a
  return $ Pull $ const $ readTMVar var

pushIn :: Exception e => IO ctx -> Push ctx a -> a -> IO (Either e ())
pushIn getContext (Push p) a = do
  c <- getContext
  atomically $ catchSTM (Right <$> p c a) (return . Left)

pullOut :: Exception e => IO ctx -> Pull ctx a -> IO (Either e a)
pullOut getContext (Pull p) = do
  c <- getContext
  atomically $ catchSTM  (Right <$> p c) (return . Left)
