{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module ParserCombinators.Practice.Json where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS (ByteString)
import Text.RawString.QQ

data TestData = TestData
  { section :: Host
  , what :: Color
  } deriving (Eq, Show)

instance FromJSON TestData where
  parseJSON (Object v) =
    TestData <$> v .: "section"
             <*> v .: "whatisit"
  parseJSON _ = fail "Expected an object for TestData!"

newtype Host = Host String deriving (Eq, Show)

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _ = fail "Expected an object for Host!"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object for Color!"

data Color =
    Red Annotation
  | Blue Annotation
  | Yellow Annotation
    deriving (Eq, Show)

type Annotation = String

sectionJson :: LBS.ByteString
sectionJson = [r|
{
  "section": {
    "host": "wikipedia.org"
  },
  "whatisit": {
    "yellow": "intoothandclaw"
  }
}
|]

parseTestData :: IO ()
parseTestData = do
  let d :: Maybe TestData
      d = decode sectionJson
  print d

