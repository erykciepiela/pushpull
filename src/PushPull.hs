module PushPull where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Data.IORef
import Data.Time

data PushContext = PushContext {
  pushTime :: UTCTime
}

getPushContext :: IO PushContext
getPushContext = PushContext <$> getCurrentTime

newtype Push a = Push (PushContext -> a -> IO ())

push :: Push a -> a -> IO ()
push (Push p) a = do
  c <- getPushContext
  p c a

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

contextualize :: (a -> PushContext -> b) -> Push b -> Push a
contextualize f (Push push) = Push $ \c a -> push c (f a c)


newtype Pull a = Pull (IO a)

pull :: Pull a -> IO a
pull (Pull p) = p

instance Functor Pull where
  fmap f (Pull p) = Pull $ fmap f p

instance Applicative Pull where
  pure = Pull . return
  Pull f <*> Pull a = Pull $ f <*> a

instance Monad Pull where
  return = pure
  (Pull p) >>= f = Pull $ do
    (Pull io) <- f <$> p
    io

extract :: (a -> b) -> Pull a -> Pull b
extract = fmap

combine :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
combine f (Pull p1) (Pull p2) = Pull $ f <$> p1 <*> p2

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
  a <- pull
  push c (a, b)

data Cell a b = Cell {
  writeCell :: Push a,
  readCell :: Pull b
}

latest :: IO (Cell a (Maybe a))
latest = do
  ref <- newIORef Nothing
  return $ Cell {
    writeCell = Push $ const $ writeIORef ref . Just,
    readCell = Pull $ readIORef ref
  }

all :: IO (Cell a [a])
all = do
  ref <- newIORef []
  return $ Cell {
    writeCell = Push $ const $ modifyIORef' ref . (:),
    readCell = Pull $ readIORef ref
  }

