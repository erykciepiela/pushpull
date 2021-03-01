module PushPull.Primitives
  ( Push
  , Pull
  , Cell
  , PushPull.Model.right
  , PushPull.Model.left
  , lifted
  , liftPush
  , unright
  , PushPull.Model.actual
  , PushPull.Model.second
  , PushPull.Model.context
  , unactual
  , existing
  , get
  , put
  , Exception
  , sequence'
  , cell
  , send
  , push
  , pull
  , map
  , split
  , ignore
  , route
  , unreach
  , mapping
  , combination
  , constant
  , selection
  , enrich
  -- , fail
  -- , failure
  , fork
  , forkN
) where

import Prelude hiding (read, id, (.), map, fail)

import PushPull.Model
import PushPull.STMExtras
import Control.Arrow
import Control.Monad.Trans.Reader

-- Push

map :: (b -> a) -> Push m ctx a -> Push m ctx b
map = contramap

split :: Applicative m => (a -> (b, c)) -> Push m ctx b -> Push m ctx c -> Push m ctx a
split = divide

-- identity for split: split f ignore p ~= p
ignore :: Applicative m => Push m ctx a
ignore = conquer

route :: Applicative m => (a -> Either b c) -> Push m ctx b -> Push m ctx c -> Push m ctx a
route = choose

-- identity to route: route f unreach p ~= p
unreach :: Applicative m => Push m ctx Void
unreach = lose id

fork :: Applicative m => Push m ctx a -> Push m ctx a -> Push m ctx a
fork = mappend

forkN :: Applicative m => [Push m ctx a] -> Push m ctx a
forkN = mconcat

-- Pull

sequence' :: (Applicative m, Traversable t) => t (Pull m a) -> Pull m (t a)
sequence' = sequenceA

-- TODO: smell, the same as in Push's map
mapping :: Functor m => Pull m a -> (a -> b) -> Pull m b
mapping = flip fmap

combination :: Applicative m => Pull m a -> Pull m b -> (a -> b -> c) -> Pull m c
combination p1 p2 f = f <$> p1 <*> p2

-- identity to combination: combination f constant p ~= p
-- identity to selection: p `selection` constant = p
constant :: Applicative m => a -> Pull m a
constant = pure

selection :: Monad m => Pull m a -> (a -> Pull m b) -> Pull m b -- 2
selection = (>>=)

-- fromContext :: Monad m => (ctx -> a) -> Pull m a
-- fromContext = arr

-- -- TODO name?
-- foo :: Monad m => Pull m ctx' -> Pull m' a -> Pull m a
-- foo = (>>>)

-- -- TODO name?
-- bar :: Monad m => (ctx -> a) -> Pull m a
-- bar = arr

-- TODO name?
-- baz :: Monad m => Pull m a -> Pull m (ctx, d) (a, d)
-- baz = Control.Arrow.first
