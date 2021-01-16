module PushPull where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

newtype Push a = Push (a -> IO ())

push :: Push a -> a -> IO ()
push (Push p) = p

instance Contravariant Push where
  contramap f (Push p) = Push (p . f)

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

insert :: (b -> a) -> Push a -> Push b -- 2
insert = contramap

infixr 0 `insert`

split :: (a -> (b, c)) -> Push b -> Push c -> Push a
split = divide

-- identity for split: split f p void ~= p
void :: Push a
void = conquer

route :: (a -> Either b c) -> Push b -> Push c -> Push a
route = choose

-- identity to route: route f p never ~= p
never :: Push Void
never = lose id

filter :: (a -> Maybe b) -> Push b -> Push a -- 2
filter f = choose (maybe (Left ()) Right . f) void


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

extract :: Pull a -> (a -> b) -> Pull b -- 2
extract = flip fmap

infixl 0 `extract`

combine :: (a -> b -> c) -> Pull a -> Pull b -> Pull c
combine f (Pull p1) (Pull p2) = Pull $ f <$> p1 <*> p2

-- identity to combine: combine f p nothing ~= p
nothing :: Pull ()
nothing = pure ()

select :: (a -> Pull b) -> Pull a -> Pull b -- 2
select = (=<<)

-- identity to select: select value p = p
value :: a -> Pull a
value = return

zip :: Pull a -> Pull b -> Pull (a, b) -- 2
zip p1 p2 = (,) <$> p1 <*> p2

