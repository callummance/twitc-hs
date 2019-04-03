{-# LANGUAGE DeriveGeneric #-}

module Twitc.Types.User where

import Data.Aeson
import GHC.Generics

-- |Data type representing a user follow: https://dev.twitch.tv/docs/api/reference/#get-users-follows
data UserFollow = UserFollow { user_follow_followed_at :: String
                             , user_follow_from_id     :: String
                             , user_follow_from_name   :: String
                             , user_follow_pagination  :: Maybe String
                             , user_follow_to_id       :: String
                             , user_follow_to_name     :: String
                             , user_follow_total       :: Maybe Int 
                             } deriving (Show, Eq, Generic)

instance ToJSON UserFollow where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop $ length "user_follow_" }

instance FromJSON UserFollow where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop $ length "user_follow_" }

-- |UserData contains publically available information on a user (as well as possibly their email): https://dev.twitch.tv/docs/api/reference/#get-users
data UserData = UserData { user_data_broadcaster_type     :: String
                         , user_data_description          :: String
                         , user_data_display_name         :: String
                         , user_data_email                :: Maybe String
                         , user_data_id                   :: String
                         , user_data_login                :: String
                         , user_data_offline_image_url    :: String
                         , user_data_profile_image_url    :: String
                         , user_data_type                 :: String
                         , user_data_view_count           :: Int
                         } deriving (Show, Eq, Generic)

instance ToJSON UserData where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop $ length "user_data_" }

instance FromJSON UserData where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop $ length "user_data_" }
