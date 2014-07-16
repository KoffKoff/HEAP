module Main where

import Prelude hiding (writeFile,readFile)
import Retrieve
import Abs
import Data.Matrix
import Services

import Data.Map
import System.Environment
import Data.Binary
import Data.ByteString.Lazy as L hiding (isSuffixOf)
import Data.List (isSuffixOf)

type Selector = String

main = do
  args <- getArgs
  case args of
    [s,fileName] -> fromAPIToFile s fileName

fromAPIToFile :: Selector -> FilePath -> IO ()
fromAPIToFile "0" file = do
  file' <- return $ if ".ed" `isSuffixOf` file then file else file ++ ".ed"
  getData tranquility charSheet (charID:testKey1) >>= writeFile file' . encode
fromAPIToFile _ _ = fail "wrong arguments"

fromFile :: FilePath -> IO EveData
fromFile file = return (if ".ed" `isSuffixOf` file then file else file ++ ".ed")
                >>= readFile >>= return . decode