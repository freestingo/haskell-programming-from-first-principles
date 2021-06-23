module MoreFunctionalPatterns.Practice.User where

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "This user is not registered!"
printUser (RegisteredUser (Username name) (AccountNumber number)) =
    putStrLn $ name ++ " " ++ show number

