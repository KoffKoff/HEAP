module Parser where

import Text.XML.Light
import Data.Time.Clock
import Control.Monad
import Data.List.Split (splitOn)

import Abs
--import Data.Matrix

data Matrix = M [String] [String] [[String]]
  deriving Show

data Parsed = Normal {name :: String, value :: Either String [Parsed], attr :: [(String,String)]}
            | Rowset {name :: String, key :: String, content :: Matrix}
  deriving Show

class Constructable r where
  construct :: Monad m => [Parsed] -> m r

-- Extracts the first XML element.
parseHttpResponse :: (Monad m) => String -> m Element
parseHttpResponse str = case parseXMLDoc str of
    Nothing -> fail "Illformed XML or something"
    Just el -> return el

-- Filters a list of contents to only elements
filterToElem :: [Content] -> [Element]
filterToElem [] = []
filterToElem (Elem el:cs) = el:filterToElem cs
filterToElem cs = (filterToElem . tail) cs

-- Extracts the (CurrentTime, list of data, cachedUntil)
getResultContent :: Monad m => Element -> m (UTCTime,[Element],UTCTime)
getResultContent el = 
  case foldl extract (Nothing,Nothing,Nothing) $ filterToElem $ elContent el of
    (Just ct, Just es, Just cu) -> return (ct,es,cu)
    _                           -> fail $ "Illformed response: " ++ show el
  where extract (ct,es,cu) e | nameEq e "result" = 
          (ct, Just (filterToElem $ elContent e), cu)
                             | nameEq e "currentTime" = 
          (getDate e, es, cu)
                             | nameEq e "cachedUntil" =
          (ct, es, getDate e)
        getDate e = case parseText e of
          Just (Normal _ (Left v) _) -> Just $ read v
          _                          -> Nothing
        nameEq e s = qName (elName e) == s

--stdParse :: (Constructable r,Monad m) => API q String (m (Result r))
--stdParse = APIResult stdParse'

-- Parser for the case when there are no attributes in the tags
stdParse :: (Constructable r, Monad m) => String -> m (Result r)
stdParse str = do
  el <- parseHttpResponse str
  (created,elems,cached) <- getResultContent el
  attrs <- mapM parseElem elems
  resultData <- construct attrs
  return $ Result created resultData cached

parseElem :: Monad m => Element -> m Parsed
parseElem el = case qName (elName el) of
  "rowset" -> parseRowset el
  _        -> parseText el

-- Extracts the text from an element
parseText :: Monad m => Element -> m Parsed
parseText el = do
  text <- case elContent el of
        [Text cd] -> return $ Left $ cdData cd
        []        -> return $ Left ""
        els       -> mapM parseElem (filterToElem els) >>= return . Right 
          --fail $ "Illformed tag: " ++ show el
  return $ Normal (qName (elName el)) text [(qName (attrKey attr),attrVal attr)
                                           | attr <- elAttribs el]

parseRowset :: Monad m => Element -> m Parsed
parseRowset el = do
  (name,key,cols) <- getRowsetVals (elAttribs el)
  mat <- constructRSMatrix key cols (filterToElem $ elContent el)
  return $ Rowset name key mat
  where getRowsetVals [n,k,c] = return (attrVal n, attrVal k, splitOn "," (attrVal c))

constructRSMatrix :: Monad m => String -> [String] -> [Element] -> m Matrix
constructRSMatrix key cols els = do
  (rows,content) <-liftM unzip $ mapM (extractStuff key cols) els
  return $ M rows cols content

extractStuff :: Monad m => String -> [String] -> Element -> m (String,[String])
extractStuff key cols el | qName (elName el) /= "row" = fail $ "Not a row: " ++ show el
                         | True = do
  let kvs = map convertToPair $ elAttribs el
  content <- foldM (foldFun kvs) [] cols
  case lookup key kvs of
    Nothing -> fail $ "No key in the row: " ++ show kvs
    Just ke -> return (ke,content)
  where foldFun kvs cont col = case lookup col kvs of
          Nothing -> fail $ "Missing value: " ++ col ++ " in row: " ++ show kvs
          Just v  -> return $ cont ++ [v]

convertToPair :: Attr -> (String,String)
convertToPair attr = (qName (attrKey attr),attrVal attr)