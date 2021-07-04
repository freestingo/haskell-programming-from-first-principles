module Monad.Practice.Cows where

data Cow = Cow
         { name   :: String
         , age    :: Int
         , weight :: Int
         } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- Bess cannot weight more than 500 as it's following a strict diet
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
       then Nothing
       else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name age weight = do
    name'   <- noEmpty name
    age'    <- noNegative age
    weight' <- noNegative weight
    weightCheck (Cow name' age' weight')

mkSphericalCow' name age weight =
    noEmpty name >>= \n -> noNegative age >>= \a -> noNegative weight >>= \w -> weightCheck (Cow n a w)

-- mkSphericalCow :: String -> Int -> Int -> Maybe Cow
-- mkSphericalCow name age weight =
--     case noEmpty name of
--       Nothing -> Nothing
--       Just n ->
--           case noNegative age of
--             Nothing -> Nothing
--             Just a ->
--                 case noNegative weight of
--                   Nothing -> Nothing
--                   Just w -> weightCheck (Cow n a w)
