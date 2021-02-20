module PushPull.Primitives
  ( Push
  , Pull
  , Exception
  , sequence'
  , variable
  , modify
  , send
  , sendBlocking
  , map
  , split
  , ignore
  , route
  , unreach
  , mapping
  , combination
  , constant
  , selection
  , context
  , enrich
  , fail
  , failure
  , fork
  , forkN
) where

import Prelude hiding (read, id, (.), map, fail)

import PushPull.Model
import PushPull.STMExtras

-- Push

map :: (b -> a) -> Push ctx a -> Push ctx b
map = contramap

split :: (a -> (b, c)) -> Push ctx b -> Push ctx c -> Push ctx a
split = divide

-- identity for split: split f ignore p ~= p
ignore :: Push ctx a
ignore = conquer

route :: (a -> Either b c) -> Push ctx b -> Push ctx c -> Push ctx a
route = choose

-- identity to route: route f unreach p ~= p
unreach :: Push ctx Void
unreach = lose id

fork :: Push ctx a -> Push ctx a -> Push ctx a
fork = mappend

forkN :: [Push ctx a] -> Push ctx a
forkN = mconcat

-- Pull

sequence' :: Traversable t => t (Pull ctx a) -> Pull ctx (t a)
sequence' = sequenceA

-- TODO: smell, the same as in Push's map
mapping :: Pull ctx a -> (a -> b) -> Pull ctx b
mapping = flip fmap

combination :: Pull ctx a -> Pull ctx b -> (a -> b -> c) -> Pull ctx c
combination p1 p2 f = f <$> p1 <*> p2

-- identity to combination: combination f constant p ~= p
-- identity to selection: p `selection` constant = p
constant :: a -> Pull ctx a
constant = pure

selection :: Pull ctx a -> (a -> Pull ctx b) -> Pull ctx b -- 2
selection = (>>=)

context :: Pull ctx ctx
context = id

fromContext :: (ctx -> a) -> Pull ctx a
fromContext = arr

-- TODO name?
foo :: Pull ctx ctx' -> Pull ctx' a -> Pull ctx a
foo = (>>>)

-- TODO name?
bar :: (ctx -> a) -> Pull ctx a
bar = arr

-- TODO name?
baz :: Pull ctx a -> Pull (ctx, d) (a, d)
baz = first
