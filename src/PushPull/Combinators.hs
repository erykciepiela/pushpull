module PushPull.Combinators where

import Prelude hiding (map, fail)
import PushPull.Primitives

-- Push

select :: (a -> Maybe b) -> Push ctx b -> Push ctx a -- TODO: smell, cannot find proper name
select f = route (maybe (Left ()) Right . f) ignore

routeIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a
routeIf f = route (\a -> (if f a then Left else Right) a)

forkIf :: (a -> Bool) -> Push ctx a -> Push ctx a -> Push ctx a -- should rely on fork not on split, as name suggests
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: (a -> Bool) -> Push ctx a -> Push ctx a
retain f p = routeIf f p ignore

remove :: (a -> Bool) -> Push ctx a -> Push ctx a
remove f = retain (not . f)

replace :: a -> Push ctx a -> Push ctx b
replace a = map (const a)

validate :: Exception e => (a -> Either e b) -> Push ctx b -> Push ctx a
validate f = route f fail

-- Pull

validated :: Exception e => (a -> Either e b) -> Pull ctx a -> Pull ctx b
validated f p = selection p (either failure return . f)

