module SignalingAdversity.Exercises.MaybeLibrary where

{-|
  Simple boolean checks for `Maybe` values.
-}
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

{-|
  The following is the `Maybe` catamorphism.
  You can turn a Maybe value into anything else with this.
-}
mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe b _ Nothing = b
mayybe b f (Just a) = f a

{-|
  In case you just want to provide a fallback value.
-}
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just a) = a

{-|
  Converting between `List` and `Maybe`.
-}
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

{-|
  For when we want to drop the Nothing values from our list.
-}
catMaybes :: [Maybe a] -> [a]
catMaybes = concatMap maybeToList

{-|
   You'll see this called "sequence" later.
-}
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs | (length . catMaybes $ xs) < length xs = Nothing
             | otherwise = Just $ catMaybes xs

