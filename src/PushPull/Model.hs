module PushPull.Model
  ( Push
  , Pull
  , variable
  , modify
  , send
  , sendBlocking
  , pushIn
  , pushOut
  , pullIn
  , pullOut
  , enrich
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
import Data.Distributive

-- Push - reads and writes state and enqueues values in contravariant/divisible/decidable way
-- pulls, updates, enqueues, verbs, commands
-- "specify the dynamic behavior of an occurence completely at the time of declaration"
-- values occuring in time, events, ephemeral, happening, discrete

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

-- ok, so we have a handful of combinators, but the only constructors we have are: conquer (ignore) and lose id (unreach)
-- therefore the question: what non-trivial constructors can we have?

modify :: TVar a -> Push ctx (a -> a)
modify v = Push $ const $ modifyTVar v

send :: TQueue a -> Push ctx a
send q = Push $ const $ writeTQueue q

sendBlocking :: TBQueue a -> Push ctx a
sendBlocking q = Push $ const $ writeTBQueue q

-- Pull - reads state in monadic way
-- variables, constants, context and derivations thereof, nouns
-- "specify the dynamic behavior of a value completely at the time of declaration"
-- values changing in time, entities, always available, lasting, continuous

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

sequence :: Traversable t => t (Pull ctx a) -> Pull ctx (t a)
sequence = sequenceA

-- ok, so we have a handful of combinators, but the only constructor we have is: pure (constant)
-- therefore the question: what non-trivial constructors can we have?

variable :: TVar a -> Pull ctx a
variable = Pull . const . readTVar


-- Push/Pull coupling

enrich :: Pull ctx b -> (a -> b -> c) -> Push ctx c -> Push ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> do
  b <- pull c
  push c $ f a b

enrich' :: (a -> Pull ctx b) -> (a -> b -> c) -> Push ctx c -> Push ctx a
enrich' s f (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull c
  push c $ f a b

enrich'' :: (a -> Pull ctx b) -> Push ctx b -> Push ctx a
enrich'' s (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull c
  push c b

enrich''' :: Pull ctx b -> Push ctx b -> Push ctx a
enrich''' (Pull pull) (Push push) = Push $ \c a -> do
  b <- pull c
  push c b


read' :: Pull ctx a -> Push ctx a -> Push ctx b
read' (Pull pull) (Push push) = Push $ \c a -> do
  a <- pull c
  push c a

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
