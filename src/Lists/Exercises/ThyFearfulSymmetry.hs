module Lists.Exercises.ThyFearfulSymmetry where

mySep :: Char -> String -> [String]
mySep _ "" = []
mySep separator string = first : mySep separator rest
                   where first = takeWhile (/= separator) string
                         rest  = dropWhile (== separator) . dropWhile (/= separator) $ string

myWords :: String -> [String]
myWords s = mySep ' ' s

myLines :: String -> [String]
myLines s = mySep '\n' s

firstSentence = "First sentence!\n"
secondSentence = "Second sentence!\n"
thirdSentence = "Third sentence!\n"
fourthSentence = "Fourth sentence!\n"

sentences = concat [ firstSentence
                   , secondSentence
                   , thirdSentence
                   , fourthSentence
                   ]

