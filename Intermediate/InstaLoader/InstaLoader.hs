{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module InstaLoader (getUserPictures, getUserProfilePic) where

import User
import Post
import Requests
import Utilities

import qualified Data.Text as T (Text, unpack, pack)
import System.Directory
import Data.Maybe
import Cp
import Control.Arrow

--------------------------------------------------------- GENERIC GETTERS -----------------------------------------------------------------

getUser :: String -- ^ Username
        -> IO User
getUser user = either error id <$> genericRequest (UserInfo user)

getUserPosts :: String -- ^ Username
             -> IO [Post]
getUserPosts user = getUserPostsRec user (UPosts user)

-- ^ Self recursive until has_more flag comes back as false.
getUserPostsRec :: String -- ^ User
                -> GRequest -- ^ Request to be made
                -> IO [Post]
getUserPostsRec user req = do
    response <- genericRequest req
    case response of 
        Left err -> error err
        Right postBatch -> do
            let new_end_cursor = T.unpack <$> (end_cursor postBatch)
                posts = collector postBatch
            if has_more postBatch then (posts ++) <$> getUserPostsRec user (NextUPosts user (fromJust new_end_cursor))
                                  else putStrLn ("Obtained " ++ (show $ count postBatch) ++ " pictures!") >> return posts

-- ^ Self recursive until has_more flag comes back as false.
-- ^ PF - Because why not?
getUserPostsRecPF :: String -- ^ User
                  -> GRequest -- ^ Request to be made
                  -> IO [Post]
getUserPostsRecPF user req = genericRequest req >>= either error (cond has_more rec end) where
    rec pb = (++ (collector pb)) <$> getUserPostsRecPF user (NextUPosts user (str_sure_end_c pb))
    rec_pf = conc <$.> rstr . ((getUserPostsRecPF user . (NextUPosts user) . str_sure_end_c) &&& collector)
    end = (\x -> putStrLn ("Obtained " ++ (show $ count x) ++ " pictures!")) >>* (return . collector)

---------------------------------------------------------------- API ----------------------------------------------------------------------
postPicToFile :: FilePath -> Post -> IO ()
postPicToFile dir post = putStrLn ("Downloading " ++ path ++ "...") >> getImg _url path where
    _url = T.unpack (url post)
    _sc = T.unpack (shortcode post)
    path = dir </> _sc ++ ".jpg"

getUserPictures :: String -- ^ Username
                -> String -- ^ Directory to be stored, if it doesn't exists one will be created
                -> IO ()
getUserPictures user dir = createDirectoryIfMissing True dir >> getUserPosts user >>= mapM_ (postPicToFile dir)

getUserProfilePicPF :: Bool -- ^ HD or not 
                    -> String -- ^ Username
                    -> String -- ^ Path to be stored
                    -> IO ()
getUserProfilePicPF hd user path = getUser user >>= (flip getImg path . T.unpack . cond (const hd) profile_pic_url_hd profile_pic_url)

getUserProfilePic :: Bool -- ^ HD or not 
                  -> String -- ^ Username
                  -> String -- ^ Path to be stored
                  -> IO ()
getUserProfilePic hd user path = do
    _user <- getUser user
    let url = if hd then profile_pic_url_hd _user else profile_pic_url _user
    getImg (T.unpack url) path