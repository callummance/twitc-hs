{-# LANGUAGE DeriveGeneric #-}

module Twitc.Types.Stream where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

-- |Data type representing information about an active stream: https://dev.twitch.tv/docs/api/reference/#get-streams
data StreamData = StreamData { stream_community_ids :: [String]
                             , stream_game_id       :: Maybe String
                             , stream_id            :: String
                             , stream_language      :: Maybe String
                             , stream_pagination    :: Maybe String
                             , stream_started_at    :: String
                             , stream_tag_ids       :: String
                             , stream_thumbnail_url :: String
                             , stream_title         :: String
                             , stream_type          :: String
                             , stream_user_id       :: String
                             , stream_user_name     :: String
                             , stream_viewer_count  :: Int
                             } deriving (Show, Eq, Generic)

instance ToJSON StreamData where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = drop $ length "stream_" }

instance FromJSON StreamData where
    parseJSON = genericParseJSON defaultOptions {
        fieldLabelModifier = drop $ length "stream_" }
