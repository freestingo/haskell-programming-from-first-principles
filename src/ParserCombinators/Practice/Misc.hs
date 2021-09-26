{-# LANGUAGE QuasiQuotes #-}
module ParserCombinators.Practice.Misc where

import Text.Trifecta
import Text.Parser.Combinators
import Text.RawString.QQ

stop :: Parser a
stop = unexpected "stop"

comb :: Parser String
comb = string "1" >> string "12" >> string "123"

one :: Parser Char
one = char '1' <* eof

one' :: Parser Char
one' = one >> stop

stopAtEOF :: Parser Char
stopAtEOF = eof >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' <* eof

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: (Show a) => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

testParse' :: (Show a) => String -> Parser a -> IO ()
testParse' s p = print $ parseString p mempty s

pnl :: String -> IO ()
pnl = putStrLn . ("\n" ++)

integ :: Parser Integer
integ = do
    i <- integer
    eof
    return i

quasiQuote :: String
quasiQuote = [r|
ciao
mamma
guarda come
mi diverto
|]

doParse :: IO ()
doParse = do
    pnl "stop:"
    -- testParse stop
    -- pnl "one:"
    -- testParse one
    -- pnl "one':"
    -- testParse one'
    -- pnl "oneTwo:"
    -- testParse oneTwo
    -- pnl "oneTwo':"
    -- testParse oneTwo'
