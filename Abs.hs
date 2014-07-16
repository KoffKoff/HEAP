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
