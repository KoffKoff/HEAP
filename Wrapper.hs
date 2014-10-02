{-# LANGUAGE FlexibleContexts #-}
module Wrapper where

import Retrieve
import Abs
import Account
import Services
import Util

callAPI :: (Parseable s r,Service s) => ReqData s -> IO (Result r)
callAPI req = do
  response <- requestData req
  parse (reqService req) response

-- Test
test :: Service s => s -> ReqData s
test s = (s,[(name,value) | (name,value) <- testKey1 ++ [charID]])

-------- Test data
charID :: (String,String)
charID = ("characterID", "94792304")

--Character key
testKey1 :: [(String,String)]
testKey1 =
  [("keyID","3523746")
  ,("vCode","f3hP9AowjgJXd9j4FuNSz8A5PGUlciG6a7t1vQvOj4jVt8TZsEFWjoSbhKxrFJCl")]

--Account Key
testKey2 :: [(String,String)]
testKey2 =
  [("keyID","3523745")
  ,("vCode","ZGNUWzSDUmoMsTiVrcM4vMQxDkzK8xD4iVTySEiLr9xKEqn3xe3cvalCXmZpdL71")]