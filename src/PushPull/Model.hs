module PushPull.Model
  ( Push
  , Pull
  , variable
  , change
  , send
  , sendBlocking
  -- , pushIn
  -- , pushOut
  -- , pullIn
  -- , pullOut
  , enrich
  -- , PushPull.Model.fail
  -- , failure
  , module Data.Void
  , module Control.Exception.Base
  , module Control.Concurrent.STM
  , module Control.Category
  , module Control.Arrow
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible
  ) where

import Prelude hiding ((.), id)

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

-- Pull - reads state in monadic way
-- variables, constants, context and derivations thereof, nouns
-- "specify the dynamic behavior of a value completely at the time of declaration"
-- values changing in time, entities, always available, lasting, continuous

newtype Pull m ctx a = Pull (ctx -> m a)

instance Monad m => Category (Pull m) where
  id = Pull return
  Pull pull2 . Pull pull1 = Pull $ pull1 >=> pull2

instance Functor m => Profunctor (Pull m) where
  rmap f (Pull pull) = Pull $ fmap f <$> pull
  lmap f (Pull pull) = Pull $ pull . f

instance Monad m => Arrow (Pull m) where
  arr f = Pull $ pure . f
  first (Pull pull) = Pull $ \(c, d) -> (,) <$> pull c <*> pure d

instance Functor m => Functor (Pull m ctx) where
  fmap = rmap

instance Applicative m => Applicative (Pull m ctx) where
  pure = Pull . const . pure
  Pull f <*> Pull a = Pull $ \c -> f c <*> a c

instance Monad m => Monad (Pull m ctx) where
  return = pure
  (Pull pull1) >>= f = Pull $ \c -> do
    Pull pull2 <- f <$> pull1 c
    pull2 c

-- non trivial contructor
variable :: m a -> Pull m ctx a
variable = Pull . const


-- Push - reads and writes state and enqueues values in contravariant/divisible/decidable way
-- pulls, updates, enqueues, verbs, commands
-- "specify the dynamic behavior of an occurence completely at the time of declaration"
-- values occuring in time, events, ephemeral, happening, discrete

newtype Push m ctx a = Push (ctx -> a -> m ())

instance Contravariant (Push m ctx) where
  contramap f (Push p) = Push $ \c -> p c . f

instance Applicative m => Divisible (Push m ctx) where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in p1 c b1 *> p2 c b2
  conquer = Push $ const $ const $ pure ()

instance Applicative m => Decidable (Push m ctx) where
  choose f (Push p1) (Push p2) = Push $ \c -> either (p1 c) (p2 c) . f
  lose f = Push $ const $ absurd . f

instance Applicative m => Semigroup (Push m ctx a) where
  p1 <> p2 = divide (\a -> (a, a)) p1 p2

instance Applicative m => Monoid (Push m ctx a) where
  mempty = conquer

-- non-trivial contructors
change :: (a -> m ()) -> Push m ctx a
change = Push . const

send :: TQueue a -> Push STM ctx a
send q = Push $ const $ writeTQueue q

sendBlocking :: TBQueue a -> Push STM ctx a
sendBlocking q = Push $ const $ writeTBQueue q

-- Push/Pull coupling

enrich :: Monad m => Pull m ctx b -> (a -> b -> c) -> Push m ctx c -> Push m ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> do
  b <- pull c
  push c $ f a b

enrich' :: Monad m => (a -> Pull m ctx b) -> (a -> b -> c) -> Push m ctx c -> Push m ctx a
enrich' s f (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull c
  push c $ f a b

enrich'' :: Monad m => (a -> Pull m ctx b) -> Push m ctx b -> Push m ctx a
enrich'' s (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull c
  push c b

enrich''' :: Monad m => Pull m ctx b -> Push m ctx b -> Push m ctx a
enrich''' (Pull pull) (Push push) = Push $ \c a -> do
  b <- pull c
  push c b


read' :: Monad m => Pull m ctx a -> Push m ctx a -> Push m ctx b
read' (Pull pull) (Push push) = Push $ \c a -> do
  a <- pull c
  push c a

-- I/O, failures

-- fail :: Exception e => Push m ctx e
-- fail = Push $ const throwSTM

-- failure :: Exception e => e -> Pull ctx a
-- failure = Pull . const . throwSTM

-- pushOut :: Int -> (a -> IO ()) -> IO (Push ctx a)
-- pushOut queueSize consume = do
--   q <- newTBQueueIO (fromIntegral queueSize)
--   forkIO $ forever $ do
--     a <- atomically $ readTBQueue q
--     consume a
--   return $ Push $ const $ writeTBQueue q

-- pullIn :: Int -> IO a -> IO (Pull ctx a)
-- pullIn periodMilliseconds producer = do
--   a <- producer
--   var <- newTMVarIO a
--   forkIO $ forever $ do
--     threadDelay $ periodMilliseconds * 1000
--     a <- producer
--     atomically $ putTMVar var a
--   return $ Pull $ const $ readTMVar var

-- pushIn :: Exception e => IO ctx -> Push ctx a -> a -> IO (Either e ())
-- pushIn getContext (Push p) a = do
--   c <- getContext
--   atomically $ catchSTM (Right <$> p c a) (return . Left)

-- pullOut :: Exception e => IO ctx -> Pull ctx a -> IO (Either e a)
-- pullOut getContext (Pull p) = do
--   c <- getContext
--   atomically $ catchSTM  (Right <$> p c) (return . Left)
