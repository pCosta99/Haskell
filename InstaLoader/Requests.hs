{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Requests where

import Control.Concurrent.Async  (Concurrently (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Network.HTTP.Simple as NS
import Network.HTTP.Client
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Types.Status  (statusCode)
import Data.Foldable (sequenceA_)
import Network.HTTP.Client.TLS
import Data.Aeson

import User
import Post

--------------------------------------- RAPID API ACCESS DATA ----------------------------------
key, rapidapi_host:: String
key = "01f6bee664msha6e890f7b55526ep1a60e9jsnabb16fac7859"
rapidapi_host = "instagram-data1.p.rapidapi.com"

headers :: NS.RequestHeaders
headers = [("x-rapidapi-key", BS.pack key), ("x-rapidapi-host", BS.pack rapidapi_host)]
--------------------------------------- RAPID API ACCESS DATA ----------------------------------
    
data GRequest = UserInfo String 
              | UPosts String
              | NextUPosts String String

data GResult = U User
             | LP [Post]
             | PB PostBatch
 
create_url :: GRequest -> String
create_url (UserInfo user) = concat ["https://", rapidapi_host, "/user/info?username=", user]
create_url (UPosts user) = concat ["https://", rapidapi_host, "/user/feed?username=", user]
create_url (NextUPosts user end_cursor) = concat ["https://", rapidapi_host, "/user/feed?username=", user, "&end_cursor=", end_cursor]

genericRequestWithManager :: (FromJSON a) -- ^ Aeson constraint
                          => GRequest -- ^ Request 
                          -> Manager -- ^ Request manager
                          -> IO (Either String a)
genericRequestWithManager req man = do
    initialRequest <- parseRequest $ create_url req
    let request = initialRequest
            { method = "GET"
            , requestHeaders = headers
            }
    response <- NS.httpLbs $ NS.setRequestManager man request
    return $ eitherDecode $ responseBody response

genericRequest :: (FromJSON a) -- ^ Aeson constraint
               => GRequest -- ^ Request
               -> IO (Either String a)
genericRequest req = newManager tlsManagerSettings >>= genericRequestWithManager req

middleware :: Manager -> GRequest -> IO GResult
middleware man req@(UserInfo _) = either error U <$> genericRequestWithManager req man
middleware man req@(UPosts _) = either error PB <$> genericRequestWithManager req man
middleware man req@(NextUPosts _ _) = either error PB <$> genericRequestWithManager req man

multipleRequests :: [GRequest] -> IO [GResult]
multipleRequests reqs = do
    manager <- newManager tlsManagerSettings
    runConcurrently . sequenceA $ map (Concurrently . middleware manager) reqs

---------------------------------------- UTILITY ----------------------------------------

getImg, getImgPf :: String -> FilePath -> IO ()
getImg url path = do
    img <- responseBody <$> (parseRequest url >>= NS.httpLBS)
    L8.writeFile path img

getImgPf url path = parseRequest url >>= NS.httpLBS >>= L8.writeFile path . responseBody