module Main where

import Lib
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


generateMsg :: IO ()
generateMsg = generateMsg' languagesToParse where
  generateMsg' (y:ys) = do
    csvData <- BL.readFile "languages.csv"
    BL.writeFile (y ++ ".msg") csvData
    removeFile (y ++ ".msg")
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
           TextIo.appendFile (y ++ ".msg") $ keyword p `append` pack(":") `append` (sel y) p `append` pack("\n\n")
    generateMsg' ys
  generateMsg' [] = return ()


main :: IO ()
main = return ()

-- TORUN generateMsg languagesToParse
