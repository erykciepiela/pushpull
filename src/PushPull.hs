module PushPull where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

newtype Push a = Push (a -> IO ())

push :: Push a -> a -> IO ()
push (Push p) = p

instance Contravariant Push where
  contramap f (Push p) = Push $ p . f

instance Divisible Push where
  divide f (Push p1) (Push p2) = Push $ \a -> let (b1, b2) = f a in do
    p1 b1
    p2 b2
  conquer = Push $ const $ return ()

instance Decidable Push where
  choose f (Push p1) (Push p2) = Push $ \a -> case f a of
    Left b1 -> p1 b1
    Right b2 -> p2 b2
  lose f = Push $ absurd . f

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

accept :: (a -> Bool) -> Push a -> Push a
accept f = choose (\a -> if f a then Right a else Left ()) void
infixr 0 `accept`

discard :: (a -> Bool) -> Push a -> Push a
discard f = accept (not . f)
infixr 0 `discard`

select :: (a -> Maybe b) -> Push b -> Push a
select f = choose (maybe (Left ()) Right . f) void
infixr 0 `select`

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
infixl 0 `extract`

combine :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
combine f (Pull p1) (Pull p2) = Pull $ f <$> p1 <*> p2

-- identity to combine: combine f nothing p ~= p
nothing :: Pull ()
nothing = pure ()

switch :: Pull a -> (a -> Pull b) -> Pull b -- 2
switch = (>>=)
infixl 0 `switch`

-- identity to switch: p `switch` always = p
always :: a -> Pull a
always = return

zip :: Pull a -> Pull b -> Pull (a, b)
zip p1 p2 = (,) <$> p1 <*> p2

