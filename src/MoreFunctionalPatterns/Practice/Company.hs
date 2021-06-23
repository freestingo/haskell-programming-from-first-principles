module MoreFunctionalPatterns.Practice.Company where

data Employee = Coder | Manager | VIP | CEO
    deriving (Eq, Ord, Show)

coderSubversion :: Employee -> Employee -> Ordering
coderSubversion Coder _ = GT
coderSubversion _ Coder = LT
coderSubversion e e' = compare e e'

rankEmployees :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
rankEmployees f e e' =
    case f e e' of
        GT -> reportBoss e e'
        EQ -> assertEquality e
        LT -> (flip reportBoss) e e'
    where reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'
          assertEquality e = putStrLn $ "These " ++ show e ++ "s share the same status"
