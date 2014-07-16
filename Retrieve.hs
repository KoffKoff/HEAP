module Retrieve ( getData ) where

import Prelude hiding (lookup)
import Network.HTTP.Conduit
import qualified Data.ByteString as S hiding (pack,unpack,map)
import qualified Data.ByteString.Char8 as SC (pack,unpack)
import Text.XML
import Data.Map as Map hiding (map,fromList)
import Data.Text (Text,pack,unpack,isPrefixOf)

import Data.Matrix as Matrix
import Services
import Abs

type Row = [(Text,Data)]

getData :: String -> String -> [(String,String)] -> IO EveData
getData server service requests = do
  response <- callEve server service $ map convert requests
  case parseLBS def (responseBody response) of
    Right doc -> parseElem $ documentRoot doc
    Left e -> fail $ show e

convert :: (String,String) -> (S.ByteString,S.ByteString)
convert (key,value) = (SC.pack key, SC.pack value)


-------- Converts XML to EVEData
parseElem :: Monad m => Element -> m EveData
parseElem (Element name attr ns) = do
  if nameLocalName name == pack "rowset"
    then parseRows ns >>= constructRS attr >>= return . Rowset
    else parseNode ns >>= return . Misc (nameLocalName name)

parseNode :: Monad m => [Node] -> m Data
parseNode [] = return Empty
parseNode (NodeContent text:xs) | isNoneData text = parseNode xs
                                | otherwise = return $ EText text
parseNode (NodeElement element:xs) = do
  parsedSuff <- parseNode xs
  case parsedSuff of
    Empty -> parseElem element >>= return . EData . flip (:) []
    (EData eds) -> do
      ed <- parseElem element
      return $ EData (ed:eds)
    _ -> fail "Evedata with Text"
parseNode _ = fail "parseNode: Found Illegal data"

-- Construction of matrix
constructRS :: Monad m => Map Name Text -> [Row] -> m Rowset
constructRS attr rows = do
  name <- first "name" attr
  key  <- first "key" attr
  return $ makeRowset name key rows
  where first k map = case Map.lookup (createName k) map of
          Just v  -> return v
          Nothing -> fail "No such element in map"
        createName name = Name (pack name) Nothing Nothing

parseRows :: Monad m => [Node] -> m [Row]
parseRows [] = return []
parseRows (NodeElement ele:xs) | nameLocalName (elementName ele) /= pack "row" =
  fail "parseRows: Not a row"
                               | otherwise = do
  parsedRows <- parseRows xs
  rowData <- parseNode $ elementNodes ele
  let row = foldrWithKey (\k v -> (:) (nameLocalName k, EText v)) [] (elementAttributes ele)
      row' = case rowData of
        Empty   -> row
        _ -> (pack "ExtraData",rowData):row
  return $ row':parsedRows
parseRows (NodeContent text:xs) | isNoneData text = parseRows xs 
                                | otherwise = fail "parseRows: Not an element"

isNoneData = isPrefixOf (pack "\r\n")


-------- Functions and data structures for contacting the EVE API
--callEve :: String -> String -> KeyValue -> IO (Response ByteString)
callEve server service body = do
  makePostRequest (server ++ service) body >>= withManager . httpLbs

--makePostRequest :: MonadThrow m => String -> [(S.ByteString,S.ByteString)] -> m Request
makePostRequest url body = parseUrl url >>= return . urlEncodedBody body

-------- Test data
charID :: (String,String)
charID = ("characterID", "94792304")

--Character key
testKey1 :: [(String,String)]
testKey1 =
  [("keyID","3523746")
  ,("vCode","f3hP9AowjgJXd9j4FuNSz8A5PGUlciG6a7t1vQvOj4jVt8TZsEFWjoSbhKxrFJCl")]

--Account Key
testKey2 :: [(String,String)]
testKey2 =
  [("keyID","3523745")
  ,("vCode","ZGNUWzSDUmoMsTiVrcM4vMQxDkzK8xD4iVTySEiLr9xKEqn3xe3cvalCXmZpdL71")]