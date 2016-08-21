{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module MonadTrans where

import Control.Applicative

main :: IO ()
main = return ()

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity

  (Identity f) <*> (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)

  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure

  (Identity a) >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure

  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  -- (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f
  --   1          2  3      8        4   5       7         6

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

hole = undefined
data Hole = Hole

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure $ pure x

  -- (<*>) :: m (a -> b) -> m a -> m b
  (<*>) :: forall a b. MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mf) <*> (MaybeT ma) = MaybeT $ liftA2 (<*>) mf ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: forall a b. MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    ma <- runMaybeT x
    case ma of
      Nothing -> return Nothing
      Just y -> runMaybeT (f y)

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT meea) = EitherT $ (fmap . fmap) f meea

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT emab) <*> (EitherT ema) = EitherT $ liftA2 (<*>) emab ema

instance Monad m => Monad (EitherT e m) where
    return = pure

    x >>= f =
        EitherT $
        do e <- runEitherT x
           case e of
               Left v -> return $ Left v
               Right v -> runEitherT (f v)

swapEither :: Either e a -> Either a e
swapEither (Left x)= Right x
swapEither (Right x)= Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ fmap swapEither ema

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)= f x
either _ g (Right x)= g x

-- newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT amb) = do
    ab <- amb
    case ab of
        Left v -> f v
        Right v -> g v

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: (a -> b) -> (ReaderT r m a) -> (ReaderT r m b)
  fmap f (ReaderT rma) = ReaderT $ (\r -> fmap f (rma r))
  -- fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT (\_ -> pure x)

  (<*>) :: forall a b. ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  -- (RoaderT rmab) <*> (ReaderT rma) = ReaderT $ (\r -> (rmab r) <*> (rma r))
  (ReaderT rmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> rmab <*> rma


instance Monad m => Monad (ReaderT r m) where
  return = pure

  -- (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  -- (ReaderT rma) >>= arrmb = ReaderT (\r -> (rma r) >>= ($r) . runReaderT . arrmb)
  (ReaderT rma) >>= f =
      ReaderT $ \r -> do
          v <- rma r
          runReaderT (f v) r

  -- m = (->) r
  -- m a -> (a -> m b) -> m b

  -- (->) r a -> (a -> (->) r b) -> (->) r b
  -- (r -> a) -> (a -> r -> b) -> (r -> b)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f (StateT sma) =
        let fmapf = \(a',s') -> (f a', s')
        in StateT $ \s -> fmap fmapf (sma s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: forall a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (f, s') <- smab s
    (x, s'') <- sma s'
    return (f x, s'')

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: forall a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  sma >>= f = StateT $ \s -> do
    (v, s') <- runStateT sma s
    runStateT (f v) s'

-- type Parser a = String -> Maybe (a, String)
-- newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
--
-- type Parser = StateT String Maybe
