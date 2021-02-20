module PushPull.Combinators where

import Prelude hiding (map, fail)
import PushPull.Primitives
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

-- Push

select :: Applicative m => (a -> Maybe b) -> Push m ctx b -> Push m ctx a -- TODO: smell, cannot find proper name
select f = route (maybe (Left ()) Right . f) ignore

routeIf :: Applicative m => (a -> Bool) -> Push m ctx a -> Push m ctx a -> Push m ctx a
routeIf f = route (\a -> (if f a then Left else Right) a)

forkIf :: Applicative m => (a -> Bool) -> Push m ctx a -> Push m ctx a -> Push m ctx a -- should rely on fork not on split, as name suggests
forkIf f thenPush = split (\a -> (if f a then Just a else Nothing, a)) (select id thenPush)

retain :: Applicative m => (a -> Bool) -> Push m ctx a -> Push m ctx a
retain f p = routeIf f p ignore

remove :: Applicative m => (a -> Bool) -> Push m ctx a -> Push m ctx a
remove f = retain (not . f)

replace :: a -> Push m ctx a -> Push m ctx b
replace a = map (const a)

-- validate :: Exception e => (a -> Either e b) -> Push m ctx b -> Push m ctx a
-- validate f = route f fail

-- Pull

-- validated :: Exception e => (a -> Either e b) -> Pull m ctx a -> Pull m ctx b
-- validated f p = selection p (either failure return . f)

