module Lists.Exercises.FilteringAndZipping where

articlesRemover :: String -> [String]
articlesRemover = filter (\x -> notElem x articles) . words
 where articles = ["the", "an", "a"]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (a : as) (b : bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a : as) (b : bs) = f a b : myZipWith f as bs

myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
