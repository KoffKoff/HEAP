{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Abs where

import Data.Time
import Data.ByteString
import Text.XML.Light

type ReqData s = (s,[(String,String)])

reqService :: ReqData s -> s
reqService = fst

reqData :: ReqData s -> [(String,String)]
reqData = snd

createReq :: Service s => s -> [(String,String)] -> ReqData s
createReq s pd = (s,pd)

data Result r = Result { created :: UTCTime, result :: r, cached :: UTCTime }
  deriving Show

class Service s where
  getService :: s -> (String, String)

class Service s => Parseable s r | s -> r where
  parse :: (Monad m) => s -> String -> m (Result r)
