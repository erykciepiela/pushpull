module PushPull.Combinators where

import Prelude hiding (id, (.), map)
import PushPull.Model
import PushPull.Primitives

-- TODO: smell, cannot find proper name
select :: (a -> Maybe b) -> Push ctx b -> Push ctx a
select f = route (maybe (Left ()) Right . f) ignore

fork :: Push ctx a -> Push ctx a -> Push ctx a
fork = mappend

forkN :: [Push ctx a] -> Push ctx a
forkN = mconcat

routeIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
routeIf f = route (\a -> (if f a then Left else Right) a)

-- should rely on fork not on split, as name suggests
forkIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: (a -> Bool) -> Push ctx a -> Push ctx a
retain f p = routeIf f p ignore

remove :: (a -> Bool) -> Push ctx a -> Push ctx a
remove f = retain (not . f)

replace :: a -> Push ctx a -> Push ctx b
replace a = map (const a)