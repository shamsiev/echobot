{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK where

import Bot
import Bot.VK.Internal
import Control.Lens ((&), (.~))
import Control.Monad (replicateM_)
import Data.Aeson (eitherDecode)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON(parseJSON), (.:), withObject)
import qualified Logger
import Network.Wreq
import qualified System.Random as Random
import qualified Web

--------------------------------------------------------------------------------
type Counter = Int

type Counters = M.Map ChatId Counter

type AccessKey = Text

type GroupId = Int

type ApiVersion = Text

type Timeout = Int

--------------------------------------------------------------------------------
data IConfig =
  IConfig
    { cAccessKey :: AccessKey
    , cGroupId :: GroupId
    , cApiVersion :: ApiVersion
    , cTimeout :: Timeout
    }
  deriving (Show)

instance FromJSON IConfig where
  parseJSON =
    withObject "FromJSON Bot.VK.IConfig" $ \o ->
      IConfig <$> o .: "access_key" <*> o .: "group_id" <*> o .: "api_version" <*>
      o .: "timeout"

--------------------------------------------------------------------------------
data IHandle =
  IHandle
    { iAccessKey :: AccessKey
    , iGroupId :: GroupId
    , iApiVersion :: ApiVersion
    , iTimeout :: Timeout
    , iHelpMessage :: Text
    , iRepeatMessage :: Text
    , iDefaultRepeat :: Counter
    , iCounters :: IORef Counters
    , iRandomState :: IORef Random.StdGen
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
new :: Logger.Handle -> Config -> IConfig -> IO Handle
new logger config iConfig = do
  counters <- newIORef M.empty
  stdGen <- Random.getStdGen
  randomState <- newIORef stdGen
  let handle =
        IHandle
          { iAccessKey = cAccessKey iConfig
          , iGroupId = cGroupId iConfig
          , iApiVersion = cApiVersion iConfig
          , iTimeout = cTimeout iConfig
          , iHelpMessage = cHelpMessage config
          , iRepeatMessage = cRepeatMessage config
          , iDefaultRepeat = cRepeatCount config
          , iCounters = counters
          , iRandomState = randomState
          , iLogger = logger
          }
  return
    Handle
      {getEvents = vkGetEvents handle, processEvents = vkProcessEvents handle}

--------------------------------------------------------------------------------
vkGetEvents :: IHandle -> IO [Event]
vkGetEvents IHandle {..} = do
  lps <- vkGetLongPollServer IHandle {..}
  Logger.info iLogger "VK: Getting updates..."
  let address = unpack $ lpsServer lps
  let options =
        defaults & param "act" .~ ["a_check"] & param "key" .~ [lpsKey lps] &
        param "wait" .~ [pack $ show iTimeout] &
        param "mode" .~ ["2"] &
        param "ts" .~ [lpsTS lps]
  (status, body) <- Web.sendOptions address options
  case status of
    200 -> do
      Logger.info iLogger "VK: Updates received"
      updates <- either fail return (eitherDecode body :: Either String Updates)
      let events = mapMaybe updateToEvent (uUpdates updates)
      return events
    _ -> fail $ "VK: Request failed: " ++ show status

--------------------------------------------------------------------------------
vkGetLongPollServer :: IHandle -> IO LongPollServer
vkGetLongPollServer IHandle {..} = do
  Logger.info iLogger "VK: Getting Long Poll Server..."
  let address = "https://api.vk.com/method/groups.getLongPollServer"
  let options =
        defaults & param "group_id" .~ [pack $ show iGroupId] &
        param "access_token" .~ [iAccessKey] &
        param "v" .~ [iApiVersion]
  (status, body) <- Web.sendOptions address options
  case status of
    200 -> do
      Logger.info iLogger "VK: Long Poll Server received"
      Logger.debug iLogger "VK: Parsing Long Poll Server"
      lps <- either fail return (eitherDecode body :: Either String LPSResponse)
      Logger.debug iLogger "VK: Parsed Long Poll Server"
      return (lpsResponse lps)
    _ -> fail $ "VK: Request failed: " ++ show status

--------------------------------------------------------------------------------
vkProcessEvents :: IHandle -> [Event] -> IO ()
vkProcessEvents handle [] = return ()
vkProcessEvents handle (e:events) = do
  case e of
    EventMessage {..} -> print "message event"
    EventMedia {..} -> print "media event"
    EventQuery {..} -> print "media event"
  vkProcessEvents handle events
