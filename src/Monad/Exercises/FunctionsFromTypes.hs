module Monad.Exercises.FunctionsFromTypes where

import Control.Monad
import Control.Applicative

{-|
   Write the following functions using the methods provided
   by `Monad` and `Functor`. Using stuff like identity and composition
   is fine, but it has to typecheck with types provided.
-}
hey :: Monad m => m (m a) -> m a
hey = join

ho :: Monad m => (a -> b) -> m a -> m b
ho = fmap

lets :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lets = liftM2

go :: Monad m => m a -> m (a -> b) -> m b
go = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) aToMB = liftA2 (++)
                            ((: []) <$> aToMB x)
                            (meh xs aToMB)

flipType :: (Monad m) => [m a] -> m [a]
flipType ms = meh ms id
