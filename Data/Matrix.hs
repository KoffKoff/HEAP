module Data.Matrix 
       ( Matrix
       , Data.Matrix.fromList 
       , lookupRow
       , lookupCol
       , Data.Matrix.lookup
       , rowNames
       , colNames
       , Data.Matrix.size
       ) where
import Prelude hiding (lookup,map)
import Data.Map hiding ((!))
import qualified Data.Map as M
import Data.Array
import qualified Data.List as L
import Data.Maybe (fromJust)

data Matrix r c v = M { rowIndex :: (Map r Int) 
                      , colIndex :: (Map c Int)
                      , matrix   :: (Array (Int,Int) v)}

instance (Show r,Show c,Show v,Ord r,Ord c) => Show (Matrix r c v) where
  show mat =
    let row0 = format 16 $ "":L.map ((:) '|' . show) (colNames mat)
        rc k v = rowConverter k $ fromJust $ flip lookupRow mat k
        rows = mapWithKey rc (rowIndex mat)
    in row0 ++ M.foldl ((++) . flip (++) "\n") "" rows
    where
      rowConverter :: (Show a,Show v) => a -> Map k v -> String
      rowConverter k m = format 16 $ show k:M.foldr ((:) . (:) '|' . show) [] m

format :: Int -> [String] -> String
format _ [] = ""
format i (x:xs) | i > 0 = take i (x ++ repeat ' ') ++ format i xs
                | True  = x ++ format i xs

fromList :: (Ord r,Ord c) => [((r,c),v)] -> Matrix r c v
fromList rcv = 
  let rMap = get fst rcv
      cMap = get snd rcv
      convert ((r,c),v) = ((rMap M.! r,cMap M.! c),v)
      matrixBounds = ((0,0),(M.size rMap - 1,M.size cMap - 1))
  in M rMap cMap $ array matrixBounds $ L.map convert rcv
  where get f = M.fromList . flip zip [0..] . L.nub . L.map (f . fst)

lookupRow :: Ord r => r -> Matrix r c v -> Maybe (Map c v)
lookupRow rName m = do
  r <- M.lookup rName $ rowIndex m
  return $ map (\c -> matrix m ! (r,c)) $ colIndex m

lookupCol :: Ord c => c -> Matrix r c v -> Maybe (Map r v)
lookupCol cName m = do
  c <- M.lookup cName $ colIndex m
  return $ map (\r -> matrix m ! (r,c)) $ rowIndex m

lookup :: (Ord r,Ord c) => r -> c -> Matrix r c v -> Maybe v
lookup rName cName m = do
  r <- M.lookup rName $ rowIndex m
  c <- M.lookup cName $ colIndex m
  return $ matrix m ! (r,c)

rowNames :: Matrix r c v -> [r]
rowNames = names rowIndex

colNames :: Matrix r c v -> [c]
colNames = names colIndex

names :: (Matrix r c v -> Map a b) -> Matrix r c v -> [a]
names = (.) (reverse . foldlWithKey (\rs r _ -> r:rs) [])

size :: Matrix r c v -> (Int,Int)
size = (\(r,c) -> (r+1,c+1)) . snd . bounds . matrix
