{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Abs where

import Data.Time
import Data.ByteString
import Text.XML.Light

type ReqData s = (s,[(String,String)])
data Result r = Result { created :: UTCTime, result :: r, cached :: UTCTime }
  deriving Show

class Service s where
  getService :: s -> (String, String)

class Service s => Parseable s r | s -> r where
  parseFun   :: (Monad m) => s -> String -> m (Result r)
