module PushPull where
import Prelude hiding (read, (.), id)

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

newtype Push ctx a = Push (ctx -> a -> STM ())

instance Contravariant (Push ctx) where
  contramap f (Push p) = Push $ \c -> p c . f

instance Divisible (Push ctx) where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in p1 c b1 >> p2 c b2
  conquer = Push $ const $ const $ return ()

instance Decidable (Push ctx) where
  choose f (Push p1) (Push p2) = Push $ \c -> either (p1 c) (p2 c) . f
  lose f = Push $ const $ absurd . f

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

-- Synonyms in FRP vocabulary

-- Push constructing, using imperative names - verbs

-- basic

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

-- shortcuts

-- TODO: smell, cannot find proper name
select :: (a -> Maybe b) -> Push ctx b -> Push ctx a
select f = route (maybe (Left ()) Right . f) ignore

fork :: Push ctx a -> Push ctx a -> Push ctx a
fork = split (\a -> (a, a))

forkN :: [Push ctx a] -> Push ctx a
forkN = foldr fork ignore

routeIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
routeIf f = route (\a -> (if f a then Left else Right) a)

-- should rely on fork not on split, as name suggests
forkIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: (a -> Bool) -> Push ctx a -> Push ctx a
retain f p = routeIf f p ignore

remove :: (a -> Bool) -> Push ctx a -> Push ctx a
remove f = retain (not . f)

enrich :: Pull ctx b -> (a -> b -> c) -> Push ctx c -> Push ctx a
enrich (Pull pull) f (Push push) = Push $ \c a -> do
  b <- pull c
  push c $ f a b

fail :: Exception e => e -> Push ctx a
fail = Push . const . const . throwSTM

validate :: Exception e => (a -> Either e b) -> Push ctx b -> Push ctx a
validate f (Push p) =  Push $ \c a -> case f a of
  Left e -> throwSTM e
  Right b -> p c b

-- Pull constructing, using declarative names: nouns

-- basic

-- TODO: smell, the same as in Push's map
extract :: (a -> b) -> Pull ctx a -> Pull ctx b
extract = fmap

combination :: (a -> b -> c) -> Pull ctx a -> Pull ctx b -> Pull ctx c
combination f (Pull p1) (Pull p2) = Pull $ \c -> f <$> p1 c <*> p2 c

-- identity to combination: combination f nothing p ~= p
nothing :: Pull ctx ()
nothing = pure ()

selection :: Pull ctx a -> (a -> Pull ctx b) -> Pull ctx b -- 2
selection = (>>=)

-- identity to selection: p `selection` constant = p
constant :: a -> Pull ctx a
constant = return

context :: Pull ctx ctx
context = id

-- TODO name?
foo :: Pull ctx ctx' -> Pull ctx' a -> Pull ctx a
foo = (>>>)

-- TODO name?
bar :: (ctx -> a) -> Pull ctx a
bar = arr

-- TODO name?
scoped :: Pull ctx a -> Pull (ctx, d) (a, d)
scoped = first

-- shortcuts

failure :: Exception e => e -> Pull ctx a
failure = Pull . const . throwSTM

-- TODO name?
valid :: Exception e => (a -> Either e b) -> Pull ctx a -> Pull ctx b
valid f (Pull p) =  Pull $ \c -> do
  a <- p c
  case f a of
    Left e -> throwSTM e
    Right b -> return b

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
    write = Push $ const $ \a -> modifyTMVar var (\(latest, previous) -> (Just a, latest)),
    read = Pull $ const $ snd <$> readTMVar var
  }

all :: IO (Cell ctx a [a])
all = atomically $ do
  var <- newTMVar []
  return $ Cell {
    write = Push $ const $ modifyTMVar var . (:),
    read = Pull $ const $ readTMVar var
  }

-- utility
modifyTMVar :: TMVar a -> (a -> a) -> STM ()
modifyTMVar var f = do
  v <- readTMVar var
  putTMVar var $ f v
