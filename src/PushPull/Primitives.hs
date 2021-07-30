module PushPull.Primitives
  ( Push
  , Pull
  , Cell
  , lifted
  , liftedIO
  , liftPush
  , get
  , put
  , cell
  , push
  , pmap
  , split
  , ignore
  , route
  , unreach
  , fork
  , forkN
) where

import PushPull.Model
import PushPull.STMExtras
import Control.Arrow
import Control.Monad.Trans.Reader
import Data.Void
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

-- Basic combinators - aliases to Contravariant/Divisible/Decidable

pmap :: (b -> a) -> Push m a -> Push m b
pmap = contramap

split :: Applicative m => (a -> (b, c)) -> Push m b -> Push m c -> Push m a
split = divide

-- identity for split: split f ignore p ~= p
ignore :: Applicative m => Push m a
ignore = conquer

route :: Applicative m => (a -> Either b c) -> Push m b -> Push m c -> Push m a
route = choose

-- identity to route: route f unreach p ~= p
unreach :: Applicative m => Push m Void
unreach = lose id

-- Other compinators

replace :: a -> Push m a -> Push m b
replace a = pmap (const a)

fork :: Applicative m => Push m a -> Push m a -> Push m a
fork = split (\a -> (a, a))

forkN :: (Applicative m, Foldable f) => f (Push m a) -> Push m a
forkN = foldr fork ignore

pfilter :: Applicative m => (a -> Maybe b) -> Push m b -> Push m a
pfilter f = route (maybe (Left ()) Right . f) ignore

ignoreWhen :: Applicative m => (a -> Bool) -> Push m a -> Push m a
ignoreWhen f = pfilter (\a -> if f a then Nothing else Just a)

ignoreWhenNot :: Applicative m => (a -> Bool) -> Push m a -> Push m a
ignoreWhenNot f = pfilter (\a -> if f a then Just a else Nothing)
