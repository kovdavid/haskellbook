{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Applicative
import Control.Monad

data Hole = Hole

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap :: (a -> b) -> (MaybeT m a -> MaybeT m b)
  fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
  pure x = MaybeT $ pure . pure $ x

  (<*>) :: forall a b. MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
  (MaybeT mab) <*> (MaybeT ma) = MaybeT $ liftA2 (<*>) mab ma

instance Monad m => Monad (MaybeT m) where
  return = pure

  (>>=) :: forall a b. (MaybeT m a) -> (a -> MaybeT m b) -> MaybeT m b
  (MaybeT mma) >>= f = MaybeT $ do
    ma :: Maybe a <- mma :: m (Maybe a)
    case ma of
      Just a -> runMaybeT $ f a
      Nothing -> return Nothing

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: forall a b . (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure . pure $ x

  (<*>) :: forall a b. EitherT e m (a -> b) -> EitherT e m a -> EitherT e m b
  (EitherT meab) <*> (EitherT mea) = EitherT $ liftA2 (<*>) meab mea

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: forall a b. EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  (EitherT mea ) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither (Right a) = Left a
swapEither (Left e) = Right e

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) = EitherT $ fmap swapEither mea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT meab) = do
  eab <- meab
  case eab of
    Left a -> f a
    Right b -> g b

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap :: forall a b. (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap ab (ReaderT (rma :: r -> m a)) = ReaderT $ \r -> fmap ab (rma r)

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \_ -> pure x

  (<*>) :: forall a b. ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (ReaderT (rmab :: r -> m (a -> b))) <*> (ReaderT (rma :: r -> m a)) =
    ReaderT $ \r -> (rmab r) <*> (rma r)

instance Monad m => Monad (ReaderT r m) where
  return = pure

  (>>=) :: forall a b. ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT (rma :: r -> m a)) >>= f = ReaderT $ \r -> do
    a :: a <- rma r
    runReaderT (f a) r

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s ->
    fmap (\(a, s') -> (f a, s')) (smas s)

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (<*>) :: forall a b. StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT smab) <*> (StateT sma) = StateT $ \s -> do
    (f, s') <- smab s
    (a, s'') <- sma s'
    return (f a, s'')

instance Monad m => Monad (StateT s m) where
  return = pure

  (>>=) :: forall a b. StateT s m a -> (a -> StateT s m b) -> StateT s m b
  (StateT sma) >>= f = StateT $ \s -> do
    (a, s') <- sma s
    runStateT (f a) s'

main :: IO ()
main = return ()
