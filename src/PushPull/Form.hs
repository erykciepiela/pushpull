module PushPull.Form where

import Prelude
import Data.IORef

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Monoid
import Control.Monad
import Control.Monad.Trans

type Dynamic r m = ReaderT r m

newtype Cell a = Cell
  { cellValueRef :: IORef a
  }

newCell :: a -> IO (Cell a)
newCell a = Cell <$> newIORef a

cellValue :: Cell a -> Dynamic r IO a
cellValue cell = ReaderT $ const (readIORef (cellValueRef cell))

displayDynamic :: r -> Dynamic r m () -> m ()
displayDynamic = flip runReaderT


type Form w r m = WriterT w (MaybeT (Dynamic r m)) -- DynamicInput

liftForm :: (Monad m, Monoid w) => Dynamic r m a -> Form w r m a
liftForm = lift . lift

data Touch = NotTouched | Touched | Touching deriving Eq

instance Semigroup Touch where
  Touching <> _ = Touching
  _ <> Touching = Touching
  Touched <> _ = Touched
  _ <> Touched = Touched
  _ <> _ = NotTouched

instance Monoid Touch where
  mempty = NotTouched

data FormField r a w = FormField
  { formFieldValueRef :: IORef (r -> Maybe (a, w))
  -- , formFieldMetaValueRef :: IORef w
  }

newFormField :: IO (FormField r a Touch)
newFormField = FormField <$> newIORef (const $ Nothing)

-- readFormField :: FormField r a w -> IO a
-- readFormField cell = readIORef (formFieldValueRef cell)

writeFormField :: FormField r a w -> (r -> Maybe (a, w)) -> IO ()
writeFormField cell = writeIORef (formFieldValueRef cell)

formFieldValue :: FormField r a w -> Form w r IO a
formFieldValue cell = WriterT $ MaybeT $ ReaderT $ \r -> do
  v <- readIORef $ formFieldValueRef cell
  return $ v r

toForm :: Monad m => m (a, w) -> Form w r m a
toForm maw = WriterT $ MaybeT $ ReaderT $ const $ Just <$> maw

justOf :: Monad m => Form w r m (Maybe a) -> Form w r m a
justOf form = WriterT $ MaybeT $ ReaderT $ \r -> do
  mmaw <- runReaderT (runMaybeT (runWriterT form)) r
  return $ case mmaw of
    Just (Just a, w) -> Just (a, w)
    _ -> Nothing

formContext :: (Monoid w, Monad m) => Form w r m r
formContext = WriterT $ MaybeT $ ReaderT $ \r -> pure $ Just (r, mempty)

withForm :: Monad m => Form w r m a -> (a -> w -> m ()) -> ReaderT r m ()
withForm form f = let rmaw = runMaybeT (runWriterT form) in do
  maw <- rmaw
  case maw of
    Just (a, w) -> ReaderT (const (f a w))
    Nothing -> return ()


withForm' :: Form w r m a -> Dynamic r m (Maybe (a, w))
withForm' form = runMaybeT (runWriterT form)


-- whenForm :: Monad m => Form w r m a -> (w -> Bool) -> (a -> m ()) -> ReaderT r m ()
whenForm form pred f = let rmaw = runMaybeT (runWriterT form) in do
  maw <- rmaw
  case maw of
    Just (a, w) -> when (pred w) $ ReaderT (const (f a))
    _ -> return ()


--
-- ctor :: Monad m => m (r -> Maybe a) -> m (r -> Maybe b) -> m (r -> Maybe c)
ctor :: Monad m => (r -> m (Maybe a)) -> (r -> m (Maybe b)) -> (r -> m (Maybe c))
ctor = undefined

type F m w r a = WriterT w (MaybeT (ReaderT r m)) a -- r -> m (Maybe a, w)

mkF :: Monad m => m (t -> Maybe (a, w)) -> WriterT w (ReaderT t (MaybeT m)) a
mkF mf = WriterT $ ReaderT $ \r -> MaybeT $ do
  f <- mf
  return $ f r

-- runF :: F m w r a -> r -> m (Maybe a)
runF f r = runMaybeT $ runReaderT f r
