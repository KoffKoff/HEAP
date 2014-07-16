module Abs ( EveData (..)
           , Rowset
           , Data (..)
           , makeRowset
           , name
           , key
           , getKeys
           , getNames
           , getValue
           , getRow
           , getCol
           ) where

import Prelude hiding (lookup)
import Data.Text hiding (take,drop,map,unlines,init,concat)
import Data.Matrix
import Data.Map hiding (map,lookup,fromList)
import Data.Binary
import Control.Monad
import Data.Text.Internal (Text(..))
import Data.Text.Array

data EveData = Rowset Rowset
             | Misc Text Data

data Rowset = RS {name :: Text, key :: Text, matrix :: Matrix Text Text Data}

data Data = Empty
          | EText Text
          | EData [EveData]

instance Show Data where
  show Empty = ""
  show (EText t) = unpack t
  show (EData d) = "Data: " ++ init (unlines (map show d))

instance Show EveData where
  show (Rowset rs) = show rs
  show (Misc name cont) = "Misc: " ++ unpack name ++ ", " 
                          ++ show cont

instance Show Rowset where
  show (RS name key matrix) = "Rowset: " ++ unpack name ++ "\n"
                              ++ take 16 (unpack key ++ repeat ' ') ++ drop 16 (show matrix)

instance Binary Data where
  put Empty     = putWord8 0
  put (EText t) = putWord8 1 >> put t
  put (EData d) = putWord8 2 >> put d
  get = do
    tag <- getWord8
    case tag of
      0 -> return Empty
      1 -> liftM EText get
      2 -> liftM EData get

instance Binary EveData where
  put (Rowset rs) = putWord8 0 >> put rs
  put (Misc n c)  = putWord8 1 >> put n >> put c
  get = do
    tag <- getWord8
    case tag of
      0 -> liftM Rowset get
      1 -> liftM2 Misc get get

instance Binary Rowset where
  put (RS n k m) = put n >> put k >> put m
  get = liftM3 RS get get get

instance Binary Text where
  put t = put $ unpack t
  get = liftM pack get

type Row = [(Text,Data)]

makeRowset :: Text -> Text -> [Row] -> Rowset
makeRowset name key rows = RS name key $ fromList rcv
  where rcv = concat $ map (prepForMatrix key) rows

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

getKeys :: Rowset -> [Text]
getKeys = rowNames . matrix

getNames :: Rowset -> [Text]
getNames = colNames . matrix

getValue :: Text -> Text -> Rowset -> Maybe Data
getValue r c = lookup r c . matrix

getRow :: Text -> Rowset -> Maybe (Map Text Data)
getRow = flip (flip lookupRow . matrix)

getCol :: Text -> Rowset -> Maybe (Map Text Data)
getCol = flip (flip lookupCol . matrix)
