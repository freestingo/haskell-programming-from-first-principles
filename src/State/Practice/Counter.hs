module State.Practice.Counter where

import Control.Monad.Trans.State

{-|
  Example use of State monad
  Passes a string of dictionary {a,b,c}
  Game is to produce a number from the string.
  By default the game is off, a C toggles the
  game on and off. A 'a' gives +1 and a b gives -1.

  E.g
  'ab'    = 0
  'ca'    = 1
  'cabca' = 0
-}

type Count = Int

type CounterState = (Bool, Count)

playGame :: String -> State CounterState Count
playGame [] = snd <$> get
playGame (x : xs) = do
  (on, count) <- get
  case x of
    'a' | on -> put (on, count + 1)
    'b' | on -> put (on, count - 1)
    'c'      -> put (not on, count)
    _        -> put (on, count)
  playGame xs

startState :: (Bool, Int)
startState = (False, 0)

computeState :: IO ()
computeState = print $ evalState (playGame "abbcaab") startState
