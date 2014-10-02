module Retrieve (requestData) where

import Network.HTTP.Conduit
import Data.ByteString as S (ByteString)
import Data.ByteString.Char8 as S (pack)
import Data.ByteString.Lazy.Char8 as L (unpack)
--import Data.Time.Clock
import Control.Monad.IO.Class

import Services
import Abs
import Util

--Creates an api request using Network.HTTP.Conduit
-- Could be generalized to 
--constructReq :: (MonadIO m, MonadBaseControl IO m, MonadThrow m, Req q) => API q (m String) r
requestData :: Service s => ReqData s -> IO String
requestData req = do
  response <- callEve (getService $ reqService req) (reqData req)
  return $ unpack $ responseBody response

-------- Functions and data structures for contacting the EVE API
--callEve :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
--           String -> Service -> [RequestAttr] -> m (Response LS.ByteString)
callEve (server, service) body = do
  makePostRequest (server ++ service) body >>= withManager . httpLbs

--makePostRequest :: MonadThrow m => String -> [(S.ByteString,S.ByteString)] -> m Request
makePostRequest url body = parseUrl url >>= return . urlEncodedBody (map packPair body)

packPair :: (String,String) -> (ByteString,ByteString)
packPair (f,s) = (pack f,pack s)
