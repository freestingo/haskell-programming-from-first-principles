module MonoidAndSemigroup.Exercises.MadLibs where

import Data.Monoid

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' excl adv noun adj = mconcat [ excl
                                       , "! he said "
                                       , adv
                                       , " as he jumped into his "
                                       , noun
                                       , " and drove off with his "
                                       , adj
                                       , " wife."
                                       ]

