
module Twitc.Hooks where

import qualified Data.Map as Map

import Control.Concurrent           (forkIO, killThread)
import Control.Concurrent.Chan      (Chan, newChan, readChan)
import Control.Concurrent.STM.TVar  (TVar, newTVarIO)
import Control.Exception            (bracket)

import Network.HTTP.Client          (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp     (run)

import Twitc.Hooks.Receive

data WebhookReceiver = WebhookReceiver { hooksMap :: TVar (Map.Map String HookPath)
                                       , messages :: Chan (HookEvent)
                                       , address  :: String
                                       }
-- |startListening takes the base web address at which webook events should be received and the port on which we should listen.
-- |The provided address is independant of the port, allowing for reverse proxies in front of the listener. As such, if a non-standard
-- | port is chosen it should also be specified as part of the address.
startListening :: String -> Int -> IO WebhookReceiver
startListening receiverAddress port = do
    mgr <- newManager defaultManagerSettings
    let initialHooks = Map.empty :: Map.Map String HookPath
    hooksVar <- newTVarIO initialHooks
    messageChannel <- newChan :: IO (Chan HookEvent)
    let runApp = run port $ app $ State {hooks = hooksVar,  backfeed = messageChannel}
    bracket (forkIO runApp) killThread $ \_ -> printEvents $ WebhookReceiver hooksVar messageChannel receiverAddress
    return $ WebhookReceiver hooksVar messageChannel receiverAddress

getEvent :: WebhookReceiver -> IO HookEvent
getEvent (WebhookReceiver _ msgs _ ) = readChan msgs

printEvents :: WebhookReceiver -> IO ()
printEvents r = do
    ev <- getEvent r
    print ev
    printEvents r
