module PushPull.Primitives where

import Prelude hiding (read, id, (.))

import PushPull.Model
import PushPull.STMExtras
--

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

fail :: Exception e => e -> Push ctx a
fail = Push . const . const . throwSTM

validate :: Exception e => (a -> Either e b) -> Push ctx b -> Push ctx a
validate f (Push p) =  Push $ \c a -> case f a of
  Left e -> throwSTM e
  Right b -> p c b

--

-- TODO: smell, the same as in Push's map
mapping :: Pull ctx a -> (a -> b) -> Pull ctx b
mapping = flip fmap

combination :: Pull ctx a -> Pull ctx b -> (a -> b -> c) -> Pull ctx c
combination (Pull p1) (Pull p2) f = Pull $ \c -> f <$> p1 c <*> p2 c

-- identity to combination: combination f constant p ~= p
-- identity to selection: p `selection` constant = p
constant :: a -> Pull ctx a
constant = return

selection :: Pull ctx a -> (a -> Pull ctx b) -> Pull ctx b -- 2
selection = (>>=)

context :: Pull ctx ctx
context = id

-- TODO name?
foo :: Pull ctx ctx' -> Pull ctx' a -> Pull ctx a
foo = (>>>)

-- TODO name?
bar :: (ctx -> a) -> Pull ctx a
bar = arr

-- TODO name?
baz :: Pull ctx a -> Pull (ctx, d) (a, d)
baz = first

failure :: Exception e => e -> Pull ctx a
failure = Pull . const . throwSTM

-- TODO name?
valid :: Exception e => (a -> Either e b) -> Pull ctx a -> Pull ctx b
valid f (Pull p) =  Pull $ \c -> do
  a <- p c
  case f a of
    Left e -> throwSTM e
    Right b -> return b

--

enrich :: Pull ctx b -> (a -> b -> c) -> Push ctx c -> Push ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> do
  b <- pull c
  push c $ f a b

-- Cell constructing

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
