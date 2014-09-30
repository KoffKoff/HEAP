{-# LANGUAGE FlexibleContexts #-}
module Main where

import Retrieve
import Abs
import Account
import Services
import Util

callAPI :: (Parseable s r,Service s) => ReqData s -> IO (Result r)
callAPI req = do
  response <- requestData req
  parseFun response

-- Test
test :: Service s => s -> ReqData s
test s = (s,[(name,value) | (name,value) <- testKey1 ++ [charID]])