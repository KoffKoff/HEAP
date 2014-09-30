{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Account where

import Data.Time

import Abs
import Retrieve
import Parser
import Services
import Util

data AccStat = AS
data KeyInfo = KI
data Characs = CL

instance Service AccStat where
  getService AS = (tranquility, accStat)
instance Service KeyInfo where
  getService KI = (tranquility, keyInfo)
instance Service Characs where
  getService CL = (tranquility, characs)

instance Parseable AccStat AccStatResult where
  parseFun AS = stdParse
instance Parseable KeyInfo APIKeyInfo where
  parseFun KI = stdParse
instance Parseable Characs Characters where
  parseFun CL = stdParse

instance Constructable AccStatResult where
  construct inputs = do
    kvs <- mapM makeDataWorkable inputs
    [pu,cd,lc,lm] <- upLooker fields kvs
    return $ AccS (read pu) (read cd) (read lc) (read lm)
    where makeDataWorkable (Normal key (Left val) _) = return (key,val)
          makeDataWorkable el = fail $ "Illconstructed element: " ++ show el
          fields = ["paidUntil","createDate","logonCount","logonMinutes"]

instance Constructable APIKeyInfo where
  construct [Normal _ (Right [rs]) att] = do
    chars <- createChars "characterName" rs
    [accMask, keyType, exp] <- upLooker fields att
    let expTime = case exp of
          "" -> startTime
          str -> read str
    return $ AccKI (read accMask) keyType expTime chars 
    where fields = ["accessMask","type","expires"]

instance Constructable Characters where
  construct [rs] = do
    createChars "name" rs >>= return . AccCL

createChars :: Monad m => String -> Parsed -> m [Character]
createChars charName (Rowset name _ (M _ cols vals))
  | name == "characters" = mapM (createChar charName cols) vals
  | otherwise = fail $ "Not a characters rowset: " ++ name
createChars _ el = fail $ "Not a rowset: " ++ show el

createChar :: Monad m => String -> [String] -> [String] -> m Character
createChar charName cols vals = do
  [chID, chNa, coID, coNa, alID, alNa, faID, faNa] <- upLooker fields (zip cols vals)
  return $ Character (read chID) chNa (read coID) coNa (read alID) alNa (read faID) faNa
  where fields = [ "characterID", charName, "corporationID"
                 , "corporationName", "allianceID", "allianceName"
                 , "factionID", "factionName"]

data AccStatResult = AccS { paidUntil     :: UTCTime
                          , createdDate   :: UTCTime
                          , logonCount    :: Int
                          , logonMinuites :: Int
                          } deriving Show
data APIKeyInfo = AccKI { accessMask :: Int
                        , keyType    :: String --Change?
                        , expires    :: UTCTime
                        , charListKI :: [Character]
                        } deriving Show
data Characters = AccCL { charList :: [Character] }
  deriving Show

data Character = Character { characterID     :: Int
                           , characterName   :: String
                           , corporationID   :: Int
                           , corporationName :: String
                           , allianceID      :: Int
                           , allianceName    :: String
                           , factionID       :: Int
                           , factionName     :: String
                           }
  deriving (Show,Read)

startTime = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)