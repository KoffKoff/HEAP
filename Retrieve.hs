module Retrieve ( EveData (..)
                , Rowset
                , Data (..)
                , Text
                , name
                ) where

import Prelude as P
import Network.HTTP.Conduit
import qualified Data.ByteString as S hiding (pack,unpack,map)
import qualified Data.ByteString.Char8 as SC (pack,unpack)
import Text.XML
import Data.Default
import Data.Map as M hiding (map)
import Data.Text as T hiding (head,unlines,init,map)
import Data.List as List (find)
import qualified Data.Maybe as May

import Data.Matrix as Mat

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

data EveData = Rowset Rowset
             | Misc Text Data

data Rowset = RS {name :: Text, key :: Text, matrix :: Matrix Text Text Data}

data Data = EEmpty
          | EText Text
          | ED [EveData]

instance Show Data where
  show EEmpty = ""
  show (EText t) = unpack t
  show (ED rs) = "Data: " ++ init (unlines (map show rs))

instance Show EveData where
  show (Rowset rs) = show rs
  show (Misc name cont) = "Misc: " ++ unpack name ++ ", " 
                          ++ show cont

instance Show Rowset where
  show (RS name key matrix) = "Rowset: " ++ unpack name ++ "\n"
                              ++ P.take 16 (unpack key ++ repeat ' ') ++ P.drop 16 (show matrix)

data APIKey = Key {keyID :: KeyID, vCode :: VCode}

type Row = [(Text,Data)]

data ParsedNode = TN Text
                | RN [Row]
                | EN [EveData]
                | Empty

--test :: [KeyValue] -> URL -> IO (Response S.ByteString)
fetchData server service body = do
  makePostRequest (server ++ service) body >>= withManager . httpLbs

--extract :: Monad m => ByteString -> m EveData
extract bs = do
  root <- parseLBS def bs >>= return . documentRoot
  parseElem root

parseElem :: Monad m => Element -> m EveData
parseElem (Element name attr ns) = do
  parsedNodes <- parseNode ns
  if name == createName "rowset"
    then getRows parsedNodes >>= constructRS attr >>= return . Rowset
    else toData parsedNodes >>= return . Misc (nameLocalName name)
  where getRows (RN rows) = return rows
        getRows Empty     = return []
        getRows _         = fail "Rowset: Malformed nodes"

toData :: Monad m => ParsedNode -> m Data
toData (TN t) = return $ EText t
toData (EN e) = return $ ED e
toData Empty  = return EEmpty
toData _      = fail "Misc: malformed nodes"

constructRS :: Monad m => Map Name Text -> [Row] -> m Rowset
constructRS attr rows = do
  name <- first (createName "name") attr
  key  <- first (createName "key") attr
  let rcv = P.concat $ map (prepForMatrix key) rows
  return $ RS name key $ Mat.fromList rcv

prepForMatrix :: Text -> Row -> [((Text,Text),Data)]
prepForMatrix k cvs = snd $ prepMat k cvs
  where prepMat k ((c,v):cvs) | k == c = 
          let EText r = v
          in (r,prepMat' r cvs)
                              | True =
          let (r,rcvs) = prepMat k cvs
          in (r,((r,c),v):rcvs)
        prepMat k [] = (k,[])
        prepMat' _ [] = []
        prepMat' r ((c,v):cvs) = ((r,c),v):prepMat' r cvs

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
  parsed <- parseNode $ elementNodes ele
  rowData <- toData parsed
  let row = foldrWithKey (\k v -> (:) (nameLocalName k, EText v)) [] (elementAttributes ele)
      row' = case rowData of
        EText _ -> (pack "ExtraData",rowData):row
        ED _    -> (pack "ExtraData",rowData):row
        EEmpty  -> row
  return $ RN (row':rs)
                               | otherwise = do
  parsedSuff <- parseNode xs
  case parsedSuff of
    Empty -> parseElem ele >>= return . EN . flip (:) []
    (EN eds) -> do
      ed <- parseElem ele
      return $ EN (ed:eds)
    _ -> fail "Evedata with other stuff"
parseNode _ = fail "Unsupported node"

first :: (Monad m,Ord k) => k -> Map k v -> m v
first k map = case M.lookup k map of
  Just v  -> return v
  Nothing -> fail "No such element in map"

--makePostRequest :: Control.Monad.Catch.MonadThrow m => String -> [KeyValue] -> m Request
makePostRequest url body = parseUrl url >>= return . urlEncodedBody body

createName :: String -> Name
createName name = Name (pack name) Nothing Nothing

showName :: Name -> Text
showName name = nameLocalName name

convert :: APIKey -> [KeyValue]
convert (Key id code) = [(SC.pack "keyID", SC.pack $ show id),(SC.pack "vCode",code)]

charID :: KeyValue
charID = (SC.pack "characterID", SC.pack "94792304")

--Character key
testKey1 :: APIKey
testKey1 = Key 3523746 $ SC.pack "f3hP9AowjgJXd9j4FuNSz8A5PGUlciG6a7t1vQvOj4jVt8TZsEFWjoSbhKxrFJCl"

--Account Key
testKey2 :: APIKey
testKey2 = Key 3523745 $ SC.pack "ZGNUWzSDUmoMsTiVrcM4vMQxDkzK8xD4iVTySEiLr9xKEqn3xe3cvalCXmZpdL71"