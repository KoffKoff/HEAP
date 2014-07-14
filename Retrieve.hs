module Retrieve where

import Network.HTTP.Conduit
import qualified Data.ByteString as S hiding (pack,unpack,map)
import qualified Data.ByteString.Char8 as SC (pack,unpack)
--import Data.ByteString.Lazy as L (pack,unpack,map)
--import Data.ByteString.Lazy as LC (pack,unpack)
import Text.XML
--import Text.XML.Cursor
import Data.Default
import Data.Map as M hiding (map)
import Data.Text as T hiding (head,unlines,init,map)
import Data.List as List (find)
import qualified Data.Maybe as May

tranquility = "https://api.eveonline.com"
--needs API key
accStatus = "/account/AccountStatus.xml.aspx"
charList = "/account/Characters.xml.aspx"
keyInfo = "/account/APIKeyInfo.xml.aspx"
--needs API key + characterID
charSheet = "/char/CharacterSheet.xml.aspx"
accBalance = "/char/AccountBalance.xml.aspx"
assets = "/char/AssetList.xml.aspx"
contacts = "/char/ContactList.xml.aspx"
contactNoti = "/char/ContactNotifications.xml.aspx"

type KeyValue = (S.ByteString,S.ByteString)
type KeyID = Int
type VCode = S.ByteString
type URL = String

data EveData = Rowset RS
             | Misc Name Data

data RS = RS Text Text [Text] [Row]
data Row = R (Map Name Text) Data

data Data = EEmpty
          | EText Text
          | ED [EveData]

instance Show Data where
  show EEmpty = ""
  show (EText t) = "Text: " ++ unpack t
  show (ED rs) = "Data:\n" ++ init (unlines (map show rs))

instance Show EveData where
  show (Rowset rs) = show rs
  show (Misc name cont) = "Misc: " ++ unpack (showName name) ++ ", " 
                          ++ show cont

instance Show RS where
  show (RS name key columns rss) = "Rowset: " ++ unpack name ++ ", key=" ++
                                   unpack key ++ showRows rss
    where showRows [] = ""
          showRows (r:rs) = "\n  " ++ show r ++ showRows rs

instance Show Row where
  show (R map rd) = "Row: " ++ foldlWithKey fixMap "[" map ++ "]" ++ show rd
    where fixMap str k v = str ++ "(" ++ unpack (showName k) ++ ", " ++ unpack v
                           ++ ")"

--data Row = R (Map String String) [EveData]

data APIKey = Key {keyID :: KeyID, vCode :: VCode}

data ParsedNode = TN Text
                | RN [Row]
                | EN [EveData]
                | Empty

--test :: [KeyValue] -> URL -> IO (Response S.ByteString)
test body url = do
  makePostRequest url body >>= withManager . httpLbs

convert :: APIKey -> [KeyValue]
convert (Key id code) = [(SC.pack "keyID", SC.pack $ show id),(SC.pack "vCode",code)]

--extract :: Monad m => ByteString -> m EveData
extract bs = do
  root <- parseLBS def bs >>= return . documentRoot
  parseElem root

parseElem :: Monad m => Element -> m EveData
parseElem (Element name attr ns) = do
  parsedNodes <- parseNode ns
  if name == createName "rowset"
    then getRows parsedNodes >>= constructRS attr >>= return . Rowset
    else excludeRows parsedNodes >>= return . Misc name
  where getRows (RN rows) = return rows
        getRows Empty     = return []
        getRows _         = fail "Rowset: Malformed nodes"

excludeRows :: Monad m => ParsedNode -> m Data
excludeRows (TN t) = return $ EText t
excludeRows (EN e) = return $ ED e
excludeRows Empty  = return EEmpty
excludeRows _      = fail "Misc: malformed nodes"

constructRS :: Monad m => Map Name Text -> [Row] -> m RS
constructRS attr rows = do
  name <- first (createName "name") attr
  key  <- first (createName "key") attr
  col  <- first (createName "columns") attr
  return $ RS name key (parseCols col) rows

parseCols :: Text -> [Text]
parseCols = splitOn $ pack ","

parseNode :: Monad m => [Node] -> m ParsedNode
parseNode [] = return Empty
parseNode (NodeContent text:xs) | pack "\r\n" `isPrefixOf` text = parseNode xs
                                | otherwise = return $ TN text
parseNode (NodeElement ele:xs) | elementName ele == createName "row" = do
  parsedSuff <- parseNode xs
  rs <- case parsedSuff of
    (RN rs) -> return rs
    Empty   -> return []
    _       -> fail "Rows and other data types"
  parsedNodes <- parseNode $ elementNodes ele
  r <- excludeRows parsedNodes
  return $ RN (R (elementAttributes ele) r:rs)
                               | otherwise = do
  parsedSuff <- parseNode xs
  case parsedSuff of
    Empty -> parseElem ele >>= return . EN . flip (:) []
    (EN eds) -> do
      ed <- parseElem ele
      return $ EN (ed:eds)
    _ -> fail "Evedata with other stuff"
parseNode _ = fail "Unsupported node"
{-
getRow ::  Monad m => ParsedNode -> m Data
getRow (EN eds) = return $ RSet $ head $ May.mapMaybe isRowset eds
getRow Empty    = return REmpty
getRow (TN t)   = return $ RText t
getRow _ = fail "Row: Not rowset"

isRowset :: EveData -> Maybe RS
isRowset (Rowset rs) = Just rs
isRowset _ = Nothing-}

first :: (Monad m,Ord k) => k -> Map k v -> m v
first k map = case M.lookup k map of
  Just v  -> return v
  Nothing -> fail "No such element in map"

--makePostRequest :: Control.Monad.Catch.MonadThrow m => String -> [KeyValue] -> m Request
makePostRequest url body = parseUrl url >>= return . urlEncodedBody body

rowset :: Name
rowset = createName "rowset"

row :: Name
row = createName "row"

createName :: String -> Name
createName name = Name (pack name) Nothing Nothing

showName :: Name -> Text
showName name = nameLocalName name

charID :: KeyValue
charID = (SC.pack "characterID", SC.pack "94792304")

--Character key
testKey1 :: APIKey
testKey1 = Key 3523746 $ SC.pack "f3hP9AowjgJXd9j4FuNSz8A5PGUlciG6a7t1vQvOj4jVt8TZsEFWjoSbhKxrFJCl"

--Account Key
testKey2 :: APIKey
testKey2 = Key 3523745 $ SC.pack "ZGNUWzSDUmoMsTiVrcM4vMQxDkzK8xD4iVTySEiLr9xKEqn3xe3cvalCXmZpdL71"