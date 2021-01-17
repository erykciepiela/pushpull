module PushPull where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad (forever)
import Control.Exception.Base

newtype Push ctx a = Push (ctx -> a -> STM ())

push :: Exception e => IO ctx -> Push ctx a -> a -> IO (Either e ())
push getContext (Push p) a = do
  c <- getContext
  atomically $ catchSTM (Right <$> p c a) (return . Left)

instance Contravariant (Push ctx) where
  contramap f (Push p) = Push $ \c -> p c . f

instance Divisible (Push ctx) where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in do
    p1 c b1
    p2 c b2
  conquer = Push $ const $ const $ return ()

instance Decidable (Push ctx) where
  choose f (Push p1) (Push p2) = Push $ \c a -> case f a of
    Left b1 -> p1 c b1
    Right b2 -> p2 c b2
  lose f = Push $ const $ absurd . f

insert :: (b -> a) -> Push ctx a -> Push ctx b
insert = contramap

split :: (a -> (b, c)) -> Push ctx b -> Push ctx c -> Push ctx a
split = divide

-- identity for split: split f void p ~= p
void :: Push ctx a
void = conquer

route :: (a -> Either b c) -> Push ctx b -> Push ctx c -> Push ctx a
route = choose

-- identity to route: route f never p ~= p
never :: Push ctx Void
never = lose id

select :: (a -> Maybe b) -> Push ctx b -> Push ctx a
select f = choose (maybe (Left ()) Right . f) void

fork :: Push ctx a -> Push ctx a -> Push ctx a
fork = split (\a -> (a, a))

forkN :: [Push ctx a] -> Push ctx a
forkN = foldr fork void

routeIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
routeIf f = route (\a -> (if f a then Left else Right) a)

forkIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: (a -> Bool) -> Push ctx a -> Push ctx a
retain f p = routeIf f p void

remove :: (a -> Bool) -> Push ctx a -> Push ctx a
remove f = retain (not . f)

contextualize :: (a -> ctx -> b) -> Push ctx b -> Push ctx a
contextualize f (Push push) = Push $ \c a -> push c (f a c)

fail :: Exception e => e -> Push ctx a
fail = Push . const . const . throwSTM

validate :: Exception e => (a -> Either e b) -> Push ctx b -> Push ctx a
validate f (Push p) =  Push $ \c a -> case f a of
  Left e -> throwSTM e
  Right b -> p c b

newtype Pull ctx a = Pull (ctx -> STM a)

pull :: Exception e => IO ctx -> Pull ctx a -> IO (Either e a)
pull getContext (Pull p) = do
  c <- getContext
  atomically $ catchSTM  (Right <$> p c) (return . Left)

instance Functor (Pull ctx) where
  fmap f (Pull p) = Pull $ fmap f . p

instance Applicative (Pull ctx) where
  pure = Pull . const . return
  Pull f <*> Pull a = Pull $ \c -> f c <*> a c

instance Monad (Pull ctx) where
  return = pure
  (Pull p) >>= f = Pull $ \c -> do
    (Pull io) <- f <$> p c
    io c

extract :: (a -> b) -> Pull ctx a -> Pull ctx b
extract = fmap

combine :: (a -> b -> c) -> Pull ctx a -> Pull ctx b -> Pull ctx c
combine f (Pull p1) (Pull p2) = Pull $ \c -> f <$> p1 c <*> p2 c

-- identity to combine: combine f nothing p ~= p
nothing :: Pull ctx ()
nothing = pure ()

switch :: Pull ctx a -> (a -> Pull ctx b) -> Pull ctx b -- 2
switch = (>>=)

-- identity to switch: p `switch` always = p
always :: a -> Pull ctx a
always = return

zip :: Pull ctx a -> Pull ctx b -> Pull ctx (a, b)
zip p1 p2 = (,) <$> p1 <*> p2

-- combining Push ctx and Pull
enrich :: Pull ctx a -> Push ctx (a, b) -> Push ctx b
enrich (Pull pull) (Push push) = Push $ \c b -> do
  a <- pull c
  push c (a, b)

contextualize' :: (a -> ctx -> b) -> Pull ctx a -> Pull ctx b
contextualize' f (Pull p) = Pull $ \c -> do
  a <- p c
  return (f a c)

fail' :: Exception e => e -> Pull ctx a
fail' = Pull . const . throwSTM

validate' :: Exception e => (a -> Either e b) -> Pull ctx a -> Pull ctx b
validate' f (Pull p) =  Pull $ \c -> do
  a <- p c
  case f a of
    Left e -> throwSTM e
    Right b -> return b

data Cell a b = Cell {
  writeCell :: forall ctx . Push ctx a,
  readCell :: forall ctx . Pull ctx b
}

latest :: IO (Cell a (Maybe a))
latest = atomically $ do
  var <- newTMVar Nothing
  return $ Cell {
    writeCell = Push $ const $ putTMVar var . Just,
    readCell = Pull $ const $ readTMVar var
  }

all :: IO (Cell a [a])
all = atomically $ do
  var <- newTMVar []
  return $ Cell {
    writeCell = Push $ const $ modifyTMVar var . (:),
    readCell = Pull $ const $ readTMVar var
  }
    where
      modifyTMVar :: TMVar a -> (a -> a) -> STM ()
      modifyTMVar var f = do
        v <- readTMVar var
        putTMVar var $ f v

mkPush :: (a -> IO ()) -> IO (Push ctx a)
mkPush io = do
    q <- newTBQueueIO 100
    forkIO $ forever $ do
      a <- atomically $ readTBQueue q
      io a
    return $ Push $ const $ writeTBQueue q

