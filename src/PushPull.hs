module PushPull where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Data.IORef
import Data.Time
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad (forever)

data PushPullContext = PushPullContext {
  currentTime :: UTCTime,
  currentUser :: String
}

getPPContext :: IO PushPullContext
getPPContext = PushPullContext <$> getCurrentTime <*> pure "anonymous user"

newtype Push a = Push (PushPullContext -> a -> STM ())

push :: Push a -> a -> IO ()
push (Push p) a = do
  c <- getPPContext
  atomically $ p c a

instance Contravariant Push where
  contramap f (Push p) = Push $ \c -> p c . f

instance Divisible Push where
  divide f (Push p1) (Push p2) = Push $ \c a -> let (b1, b2) = f a in do
    p1 c b1
    p2 c b2
  conquer = Push $ const $ const $ return ()

instance Decidable Push where
  choose f (Push p1) (Push p2) = Push $ \c a -> case f a of
    Left b1 -> p1 c b1
    Right b2 -> p2 c b2
  lose f = Push $ const $ absurd . f

insert :: (b -> a) -> Push a -> Push b
insert = contramap
infixr 0 `insert`

split :: (a -> (b, c)) -> Push b -> Push c -> Push a
split = divide

-- identity for split: split f void p ~= p
void :: Push a
void = conquer

route :: (a -> Either b c) -> Push b -> Push c -> Push a
route = choose

-- identity to route: route f never p ~= p
never :: Push Void
never = lose id

select :: (a -> Maybe b) -> Push b -> Push a
select f = choose (maybe (Left ()) Right . f) void

fork :: Push a -> Push a -> Push a
fork = split (\a -> (a, a))

forkN :: [Push a] -> Push a
forkN = foldr fork void

routeIf :: (a -> Bool) -> Push a -> Push a -> Push a
routeIf f = route (\a -> (if f a then Left else Right) a)

forkIf :: (a -> Bool) -> Push a -> Push a -> Push a
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: (a -> Bool) -> Push a -> Push a
retain f p = routeIf f p void

remove :: (a -> Bool) -> Push a -> Push a
remove f = retain (not . f)

contextualize :: (a -> PushPullContext -> b) -> Push b -> Push a
contextualize f (Push push) = Push $ \c a -> push c (f a c)

newtype Pull a = Pull (PushPullContext -> STM a)

pull :: Pull a -> IO a
pull (Pull p) = do
  c <- getPPContext
  atomically $ p c

instance Functor Pull where
  fmap f (Pull p) = Pull $ fmap f . p

instance Applicative Pull where
  pure = Pull . const . return
  Pull f <*> Pull a = Pull $ \c -> f c <*> a c

instance Monad Pull where
  return = pure
  (Pull p) >>= f = Pull $ \c -> do
    (Pull io) <- f <$> p c
    io c

extract :: (a -> b) -> Pull a -> Pull b
extract = fmap

combine :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
combine f (Pull p1) (Pull p2) = Pull $ \c -> f <$> p1 c <*> p2 c

-- identity to combine: combine f nothing p ~= p
nothing :: Pull ()
nothing = pure ()

switch :: Pull a -> (a -> Pull b) -> Pull b -- 2
switch = (>>=)

-- identity to switch: p `switch` always = p
always :: a -> Pull a
always = return

zip :: Pull a -> Pull b -> Pull (a, b)
zip p1 p2 = (,) <$> p1 <*> p2

-- combining Push and Pull
enrich :: Pull a -> Push (a, b) -> Push b
enrich (Pull pull) (Push push) = Push $ \c b -> do
  a <- pull c
  push c (a, b)

contextualize' :: (a -> PushPullContext -> b) -> Pull a -> Pull b
contextualize' f (Pull p) = Pull $ \c -> do
  a <- p c
  return (f a c)

data Cell a b = Cell {
  writeCell :: Push a,
  readCell :: Pull b
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

mkPush :: (a -> IO ()) -> IO (Push a)
mkPush io = do
    q <- newTBQueueIO 100
    forkIO $ forever $ do
      a <- atomically $ readTBQueue q
      io a
    return $ Push $ const $ writeTBQueue q

