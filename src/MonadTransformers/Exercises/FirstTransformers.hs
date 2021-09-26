{-# LANGUAGE TupleSections #-}
module MonadTransformers.Exercises.FirstTransformers where

import Data.Bifunctor (first)
import Control.Monad (liftM)
import Control.Monad.Trans.Class

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure
    (MaybeT fab) <*> (MaybeT mma) = MaybeT $ fmap (<*>) fab <*> mma

instance Monad m => Monad (MaybeT m) where
    return = pure
    (MaybeT ma) >>= f = MaybeT $ do
        mayyyybe <- ma
        case mayyyybe of
          Just val -> runMaybeT $ f val
          Nothing -> return Nothing

{-|
    Write the Functor, Applicative and Monad instance for EitherT.
-}
newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT ea) = EitherT $ (fmap . fmap) f ea

instance Applicative m => Applicative (EitherT e m) where
    pure = EitherT . pure . pure
    (EitherT eaf) <*> (EitherT ea) = EitherT $ (<*>) <$> eaf <*> ea

instance Monad m => Monad (EitherT e m) where
    return = pure
    (EitherT ea) >>= f = EitherT $ do
        eitherVal <- ea
        case eitherVal of
          Right val -> runEitherT $ f val
          Left val -> return $ Left val

instance MonadTrans (EitherT e) where
  lift = EitherT . fmap Right

{-|
   Write the swapEitherT helper function for EitherT.
-}
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ea) = EitherT $ fmap swapEither ea

swapEither :: Either e a -> Either a e
swapEither (Left a) = Right a
swapEither (Right e) = Left e

{-|
   Write the transformer variant of the either catamorphism.
-}
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT aToMC bToMC (EitherT ab) = do
    eitherVal <- ab
    case eitherVal of
      Left val -> aToMC val
      Right val -> bToMC val

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT sma) = StateT $ fmap (first f) . sma

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)
  (StateT sToMAToBS) <*> (StateT sToMAS) = StateT $ \state -> do
    (aToB, s) <- sToMAToBS state
    (a, s') <- sToMAS state
    return (aToB a, s')

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT sma) >>= f = StateT $ \state -> do
    (a, s) <- sma state
    runStateT (f a) s

instance MonadTrans (StateT s) where
  lift ma = StateT $ \state -> fmap (, state) ma






