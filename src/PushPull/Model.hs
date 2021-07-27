module PushPull.Model
  ( Push
  , Pull
  , Cell
  , get
  , put
  , lifted
  , liftedIO
  , liftPush
  , cell
  , send
  , push
  , module Data.Void
  , module Control.Exception.Base
  , module Control.Concurrent.STM
  , module Control.Category
  , module Data.Functor.Contravariant
  , module Data.Functor.Contravariant.Divisible
  ) where

import Prelude hiding ((.), id)

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
-- import Data.Profunctor
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
import Data.Either.Combinators
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

-- Pull - reads state in monadic way
-- Represents variables, constants, context and derivations thereof, nouns,
-- values changing in time, entities, continuously lasting values.

newtype Pull m a = Pull { pull :: m a }

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

lifted :: (MonadTrans t, Monad m) => Pull m a -> Pull (t m) a
lifted (Pull p) = Pull (lift p)

liftedIO :: MonadIO m => Pull IO a -> Pull m a
liftedIO (Pull p) = Pull (liftIO p)

-- Push - writes state and enqueues values in contravariant/divisible/decidable way
-- Represents pulls, updates, enqueues, verbs, commands,
-- values occuring in time, events, ephemeral, happening, discrete values

newtype Push m a = Push { push :: a -> m () }

instance Contravariant (Push m) where
  contramap f (Push p) = Push $ p . f

instance Applicative m => Divisible (Push m) where
  divide f (Push p1) (Push p2) = Push $ \a -> let (b1, b2) = f a in (<>) <$> p1 b1 <*> p2 b2
  conquer = Push $ const $ pure ()

instance Applicative m => Decidable (Push m) where
  choose f (Push p1) (Push p2) = Push $ either p1 p2 . f
  lose f = Push $ absurd . f

instance Applicative m => Semigroup (Push m a) where
  p1 <> p2 = divide (\a -> (a, a)) p1 p2

instance Applicative m => Monoid (Push m a) where
  mempty = conquer

liftPush :: (MonadTrans t, Monad m) => Push m a -> Push (t m) a
liftPush (Push p) = Push $ \s -> lift (p s)

liftIOPush :: MonadIO m => Push IO a -> Push m a
liftIOPush (Push p) = Push $ \s -> liftIO (p s)

data Cell m a = Cell {
  cellName :: String,
  put :: Push m a,
  get :: Pull m a
}

-- non-trivial contructors

cell :: Applicative m => String -> (a -> m ()) -> m a -> Cell m a
cell name put get = Cell name (Push $ \a -> put a $> ()) (Pull get)

send :: MonadIO m => (a -> IO ()) -> Push m a
send am = Push $ \a -> liftIO (am a) $> ()

read' :: Monad m => Pull m a -> Push m a -> Push m b
read' (Pull pull) (Push push) = Push $ \a -> do
  a <- pull
  push a

type Foo m a = Push (Pull m) a

type Bar m a = Pull (Push m) a
