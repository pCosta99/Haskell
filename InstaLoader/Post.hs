{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Post where

import Data.Aeson  
import GHC.Generics
import qualified Data.Text as T (Text, unpack, pack)
import Data.Maybe
import Cp

data Post =
  Post { shortcode :: !T.Text
        , is_video :: !Bool
        , height :: !Int
        , width :: !Int
        , url :: !T.Text
        , description :: !T.Text
        , comments :: !Int
        , hashtags :: ![T.Text]
        , location :: !(Maybe T.Text)
  } deriving (Show, Generic)

instance FromJSON Post where
    parseJSON = withObject "Post" $ \v -> Post
        <$> v .: "shortcode"
        <*> v .: "is_video"
        <*> ((v .: "dimension") >>= (.: "height"))
        <*> ((v .: "dimension") >>= (.: "width"))
        <*> v .: "display_url"
        <*> v .: "description"
        <*> v .: "comments"
        <*> v .: "hashtags"
        <*> ((v .:? "location") >>= (cond isNothing (return . nothing) $ (.:? "name") . fromJust))

instance ToJSON Post 

data PostBatch =
    Batch { count :: !Int
          , has_more :: !Bool
          , end_cursor :: !(Maybe T.Text)
          , collector :: ![Post]
    } deriving (Show, Generic)

instance FromJSON PostBatch
instance ToJSON PostBatch

str_sure_end_c :: PostBatch -> String
str_sure_end_c = T.unpack . fromJust . end_cursor