{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Twitc.Hooks.Receive where


import Debug.Trace
import Servant
import Data.Aeson
import Data.Aeson.Types

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.Chan
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Time.Clock

import Twitc.Types.Stream (StreamData)
import Twitc.Types.User   (UserData, UserFollow)

type TwitchHookReceiverAPI = SubscriptionConfirmEndpoint :<|> TwitchHookReceiveAPI

type TwitchHookReceiveAPI = "user_follows"            :> Capture "subid" String :> ReqBody '[JSON] (HookNotification UserFollow)             :> Post '[JSON] NoContent
                       :<|> "stream_change"           :> Capture "subid" String :> ReqBody '[JSON] (HookNotification StreamData)             :> Post '[JSON] NoContent
                       :<|> "user_change"             :> Capture "subid" String :> ReqBody '[JSON] (HookNotification UserData)               :> Post '[JSON] NoContent
                       -- :<|> "game_analytics"          :> Capture "subid" String :> ReqBody '[JSON] (HookNotification GameAnalyticsData)      :> Post '[JSON] NoContent
                       -- :<|> "extension_analytics"     :> Capture "subid" String :> ReqBody '[JSON] (HookNotification ExtensionAnalyticsData) :> Post '[JSON] NoContent

-- |EventType contains details on which type of notification was triggered
data EventType = UserFollowEventType
               | StreamChangeEventType
               | UserChangeEventType
               deriving (Show, Eq)

data HookEvent = UserFollowEvent UserFollow
               | StreamChangeEvent StreamData
               | UserChangeEvent UserData
               | HookRejection String String -- Id Reason
               | HookConfirmed String -- Id
               | UnknownEvent
               deriving (Show, Eq)

-- |HookPath contains important information on a single hook
data HookPath = HookPath { hookId        :: String
                         , hookEventType :: EventType
                         , hookSecret    :: String
                         , expires       :: UTCTime
                         , hookTopic     :: String
                         } deriving (Show, Eq)

data State = State { hooks      :: TVar (Map.Map String HookPath)
                   , backfeed   :: Chan (HookEvent)
                   } deriving (Eq)

type AppM = ReaderT State Handler

type SubscriptionConfirmEndpoint = "confirm_subscription" :> Capture "subid" String
                                                          :> QueryParam "hub.mode" SubscriptionMode 
                                                          :> QueryParam "hub.topic" String 
                                                          :> QueryParam "hub.lease_seconds" Int 
                                                          :> QueryParam "hub.challenge" String 
                                                          :> QueryParam "hub.reason" String
                                                          :> Get '[PlainText] String

data SubscriptionMode = Subscribe | Unsubscribe | Denied
instance FromHttpApiData SubscriptionMode where
    parseUrlPiece "subscribe"   = Right Subscribe
    parseUrlPiece "unsubscribe" = Right Unsubscribe
    parseUrlPiece "denied"      = Right Denied
    parseUrlPiece m             = Left ("Unknown Subscription Mode" <> m)


data HookNotification a = NoNotification
                        | Notification [a]
                        deriving(Show, Eq)

instance FromJSON a => FromJSON (HookNotification a) where
    parseJSON = withObject "notification" $ \o -> do
        payloadList <- o .: "data"
        traceShowM payloadList
        case isEmptyArray payloadList of
             True  -> return NoNotification
             False -> fmap Notification $ parseJSON payloadList

api :: Proxy TwitchHookReceiverAPI
api = Proxy

confirmSubscription :: String -> Maybe SubscriptionMode -> Maybe String -> Maybe Int -> Maybe String -> Maybe String -> AppM String
confirmSubscription subid (Just Subscribe) (topic) (Just lease_secs) (Just challenge) reason = do
    State{hooks = hs, backfeed = f} <- ask
    now <- liftIO $ getCurrentTime
    let expires_time = addUTCTime (realToFrac lease_secs) now
    -- TODO: check that subscriptions were expected in order to prevent potential DoS
    liftIO $ atomically $ do
        hooksMap <- readTVar hs
        writeTVar hs $ Map.adjust (\h -> h {expires = expires_time}) subid hooksMap
    liftIO $ writeChan f $ HookConfirmed subid
    return challenge

confirmSubscription subid mode topic lease_secs challenge reason = do
    let givenReason = case reason of
                            Just r   -> r
                            Nothing  -> ""
    State{hooks = hs, backfeed = f} <- ask
    liftIO $ writeChan f $ HookRejection subid givenReason
    return ""

userFollow :: String -> HookNotification UserFollow -> AppM NoContent
userFollow subid (Notification ns) = do
    State{hooks = hs, backfeed = f} <- ask
    liftIO $ writeChan f $ event
    return NoContent
        where
            notification = head ns --Assuming there will only be one bit of data in the array
            event = UserFollowEvent notification

streamChange :: String -> HookNotification StreamData -> AppM NoContent
streamChange subid (Notification ns) = do
    State{hooks = hs, backfeed = f} <- ask
    liftIO $ writeChan f $ event
    return NoContent
        where
            notification = head ns --Assuming there will only be one bit of data in the array
            event = StreamChangeEvent notification

userChange :: String -> HookNotification UserData -> AppM NoContent
userChange subid (Notification ns) = do
    State{hooks = hs, backfeed = f} <- ask
    liftIO $ writeChan f $ event
    return NoContent
        where
            notification = head ns --Assuming there will only be one bit of data in the array
            event = UserChangeEvent notification

server :: ServerT TwitchHookReceiverAPI AppM
server = confirmSubscription :<|> userFollow :<|> streamChange :<|> userChange

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve api $ hoistServer api (nt s) server
