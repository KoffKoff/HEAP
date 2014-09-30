module Util where

import Abs

reqService :: ReqData s -> s
reqService = fst

reqData :: ReqData s -> [(String,String)]
reqData = snd

upLooker :: Monad m => [String] -> [(String,String)] -> m [String]
upLooker names kv = case mapM (flip lookup kv) names of
  Just vals -> return vals
  _ -> fail $ "missing one or more value:\n" ++ show kv
           ++ "\nfrom the keys:\n" ++ show names
