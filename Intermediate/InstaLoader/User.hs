{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module User where

import Data.Aeson  
import GHC.Generics
import qualified Data.Text as T (Text, unpack, pack)

data User =
  User { biography :: !T.Text
        , followed_by :: !Int
        , following :: !Int
        , full_name :: !T.Text
        , is_private :: !Bool
        , username :: !T.Text
        , profile_pic_url :: !T.Text
        , profile_pic_url_hd :: !T.Text
          } deriving (Show, Generic)

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> User
        <$> v .: "biography"
        <*> ((v .: "edge_followed_by") >>= (.: "count"))
        <*> ((v .: "edge_follow") >>= (.: "count"))
        <*> v .: "full_name"
        <*> v .: "is_private"
        <*> v .: "username"
        <*> v .: "profile_pic_url"
        <*> v .: "profile_pic_url_hd"
instance ToJSON User 