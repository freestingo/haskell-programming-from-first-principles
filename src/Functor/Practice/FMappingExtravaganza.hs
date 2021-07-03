module Functor.Practice.FMappingExtravaganza where

{-|
    Functors are stackable!!!!
-}
msl :: [Maybe String]
msl = [Just "we we", Nothing, Just "allora io vado"]

replaceWithP :: a -> Char
replaceWithP _ = 'p'

{-|
   Try in the REPL:
     replaceWithP msl
     fmap replaceWithP msl
     (fmap . fmap) replaceWithP msl
     (fmap . fmap . fmap) replaceWithP msl
-}

