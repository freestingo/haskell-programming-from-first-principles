module MonadTransformers.Exercises.Misc where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Identity

rDec :: Num a => Reader a a
rDec = asks (+ (-1))

rShow :: Show a => ReaderT a Identity String
rShow = asks show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \x -> do
  putStrLn $ "Hello, " <> show x
  return $ x + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \x -> do
  putStrLn $ "Hello, " <> show x
  return (show x, x + 1)


