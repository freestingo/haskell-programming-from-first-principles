module Lists.Practice.ListComprehensions where

acro :: String -> String
acro xs = [ x | x <- xs
              , elem x ['A'..'Z']
          ]

