module PushPull.Model
  ( Push
  , Pull
  , Cell
  , get
  , put
  , PushPull.Model.right
  , lifted
  , liftPush
  , PushPull.Model.unright
  , PushPull.Model.left
  , PushPull.Model.actual
  , PushPull.Model.unactual
  , PushPull.Model.second
  , existing
  , cell
  , send
  , push
  , pull
  , context
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
import qualified Control.Arrow as A
import PushPull.STMExtras
import Data.Functor
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Reader
import Data.Either.Combinators
import Control.Monad.Trans.Writer

-- Pull - reads state in monadic way
-- variables, constants, context and derivations thereof, nouns
-- "specify the dynamic behavior of a value completely at the time of declaration"
-- values changing in time, entities, always available, lasting, continuous

newtype Pull m a = Pull (m a)

context :: Monad m => Pull (ReaderT ctx m) ctx
context = Pull (ReaderT return)

lifted :: (MonadTrans t, Monad m) => Pull m a -> Pull (t m) a
lifted (Pull p) = Pull (lift p)

-- if m is a monad then (m (Either e a)) is a monad in a
right :: Pull m (Either e a) -> Pull (ExceptT e m) a
right (Pull p) = Pull (ExceptT p)

left :: Functor m => Pull m (Either a b) -> Pull (MaybeT m) a
left e = existing $ leftToMaybe <$> e

unright :: Pull (ExceptT e m) a -> Pull m (Either e a)
unright (Pull p) = Pull (runExceptT p)

-- if m is a monad then (m (Maybe a)) is a monad in a
existing :: Pull m (Maybe a) -> Pull (MaybeT m) a
existing (Pull p) = Pull (MaybeT p)

unexisting :: Pull (MaybeT m) a -> Pull m (Maybe a)
unexisting (Pull p) = Pull (runMaybeT p)

-- if m is a monad and w is a monoid then m (a, w) is a monad in a
actual :: Pull m (a, w) -> Pull (WriterT w m) a
actual (Pull p) = Pull (WriterT p)

second :: Functor m => Pull m (a, w) -> Pull m w
second (Pull p) = Pull (fmap snd p)

unactual :: Pull (WriterT w m) a -> Pull m (a, w)
unactual (Pull p) = Pull (runWriterT p)

instance Functor m => Functor (Pull m) where
  fmap f (Pull p)= Pull $ f <$> p

instance Applicative m => Applicative (Pull m) where
  pure = Pull . pure
  Pull f <*> Pull a = Pull $ f <*> a

instance Monad m => Monad (Pull m) where
  return = pure
  (Pull pull1) >>= f = Pull $ do
    Pull pull2 <- f <$> pull1
    pull2

-- Push - reads and writes state and enqueues values in contravariant/divisible/decidable way
-- pulls, updates, enqueues, verbs, commands
-- "specify the dynamic behavior of an occurence completely at the time of declaration"
-- values occuring in time, events, ephemeral, happening, discrete

type Failure = String

newtype Push m ctx a = Push (ctx -> a -> m ([Failure], [String]))

instance Contravariant (Push m ctx) where
  contramap f (Push p) = Push $ \c -> p c . f

instance Applicative m => Divisible (Push m ctx) where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in (<>) <$> p1 c b1 <*> p2 c b2
  conquer = Push $ const $ const $ pure ([], [])

instance Applicative m => Decidable (Push m ctx) where
  choose f (Push p1) (Push p2) = Push $ \c -> either (p1 c) (p2 c) . f
  lose f = Push $ const $ absurd . f

instance Applicative m => Semigroup (Push m ctx a) where
  p1 <> p2 = divide (\a -> (a, a)) p1 p2

instance Applicative m => Monoid (Push m ctx a) where
  mempty = conquer

fail :: Applicative m => Push m ctx Failure
fail = Push $ const $ \f -> pure ([f], [])

validate :: Applicative m => (a -> Either Failure b) -> Push m ctx b -> Push m ctx a
validate f = choose f PushPull.Model.fail

guard :: Applicative m => (a -> Bool) -> Push m ctx a -> Push m ctx a
guard f = undefined -- choose f PushPull.Model.fail

liftPush :: (MonadTrans t, Monad m) => Push m ctx a -> Push (t m) ctx a
liftPush (Push p) = Push $ \ctx s -> lift (p ctx s)

data Cell m a = Cell {
  cellName :: String,
  put :: forall ctx . Push m ctx a,
  get :: forall ctx . Pull m a
}

-- non-trivial contructors

cell :: Applicative m => String -> (a -> m ()) -> m a -> Cell m a
cell name put get = Cell name (Push $ const $ \a -> put a $> ([], [name])) (Pull get)

send :: Functor m => (a -> m ()) -> Push m ctx a
send am = Push $ const $ \a -> am a $> ([], [])

-- Push/Pull coupling

enrich :: Monad m => Pull m b -> (a -> b -> c) -> Push m ctx c -> Push m ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> (do
  b <- pull
  push c $ f a b)

enrich' :: Monad m => (a -> Pull m b) -> (a -> b -> c) -> Push m ctx c -> Push m ctx a
enrich' s f (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull
  push c $ f a b

enrich'' :: Monad m => (a -> Pull m b) -> Push m ctx b -> Push m ctx a
enrich'' s (Push push) = Push $ \c a -> do
  let (Pull pull) = s a
  b <- pull
  push c b

enrich''' :: Monad m => Pull m b -> Push m ctx b -> Push m ctx a
enrich''' (Pull pull) (Push push) = Push $ \c a -> do
  b <- pull
  push c b


read' :: Monad m => Pull m a -> Push m ctx a -> Push m ctx b
read' (Pull pull) (Push push) = Push $ \c a -> do
  a <- pull
  push c a

-- I/O, failures

pull :: Pull m a -> m a
pull (Pull p) = p

push :: Push m ctx a -> ctx -> a -> m ([Failure], [String])
push (Push p) = p

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
