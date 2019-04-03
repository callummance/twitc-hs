{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitc.Hooks.Receive where

import Servant

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Twitc.Types.Stream (StreamData)
import Twitc.Types.User   (UserData, UserFollow)

type TwitchHookRecieverAPI = SubscriptionConfirmEndpoint :<|> TwitchHookReceiveAPI

type TwitchHookReceiveAPI = "user_follows"            :> Capture "subid" :> ReqBody '[JSON] (HookNotification UserFollow)             :> Post '[JSON] NoContent
                       :<|> "stream_change"           :> Capture "subid" :> ReqBody '[JSON] (HookNotification StreamData)             :> Post '[JSON] NoContent
                       :<|> "user_change"             :> Capture "subid" :> ReqBody '[JSON] (HookNotification UserData)               :> Post '[JSON] NoContent
                       -- :<|> "game_analytics"          :> Capture "subid" :> ReqBody '[JSON] (HookNotification GameAnalyticsData)      :> Post '[JSON] NoContent
                       -- :<|> "extension_analytics"     :> Capture "subid" :> ReqBody '[JSON] (HookNotification ExtensionAnalyticsData) :> Post '[JSON] NoContent



type SubscriptionConfirmEndpoint = "confirm_subscription" :> Capture "subid" 
                                                          :> QueryParam "hub.mode" SubscriptionMode 
                                                          :> QueryParam "hub.topic" String 
                                                          :> QueryParam "hub.lease_seconds" Int 
                                                          :> QueryParam "hub.challenge" String 
                                                          :> Get '[PlainText] String

data SubscriptionMode = Subscribe | Unsubscribe | Denied
instance FromHttpApiData SubscriptionMode where
    parseUrlPiece "subscribe"   = Right Subscribe
    parseUrlPiece "unsubscribe" = Right Unsubscribe
    parseUrlPiece "denied"      = Right Denied
    parseUrlPiece m             = Left ("Unknown Subscription Mode" `T.append` m)


data HookNotification a = NoNotification
                        | Notification [a]
                        deriving(Show, Eq)

instance FromJSON a => FromJSON (HookNotification a) where
    parseJSON = withObject "notification" $ \o -> do
        payloadList <- o .: "data"
        case payloadList of
             --emptyArray -> return NoNotification
             _          -> fmap Notification $ parseJSON payloadList

