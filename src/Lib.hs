{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where


import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import System.Directory
import Data.Text
import Text.Parsec
import Text.Read
import qualified Data.Text.IO as TextIo
import qualified Data.ByteString.Lazy as BL
import Data.Text.Encoding


data Language = Language
    { keyword  :: !Text
    , en :: !Text
    , ge :: !Text
    , it :: !Text
    , hr :: !Text
    }
    deriving (Show)

instance FromNamedRecord Language where
    parseNamedRecord r = Language <$> r .: "keyword"
        <*> r .: "hr" <*> r .: "ge" <*> r .: "it" <*> r .: "en"

sel :: String -> (Language -> Text)
sel "hr" = hr
sel "en" = en
sel "it" = it
sel "ge" = ge

languagesToParse = ["hr","en","it","ge"]
