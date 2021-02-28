{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK where

import Bot
import Bot.VK.Internal
import Control.Lens ((&), (.~))
import Data.Aeson (eitherDecode)
import Data.IORef (IORef)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
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

type ApiVersion = Int

type Timeout = Int

--------------------------------------------------------------------------------
data IConfig =
  IConfig
    { cAccessKey :: AccessKey
    , cGroupId :: GroupId
    , cApiVersino :: ApiVersion
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
    , iRepeatCount :: Int
    , iCounters :: IORef Counters
    , iRandomState :: IORef Random.StdGen
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
vkGetEvents :: IHandle -> IO [Event]
vkGetEvents IHandle {..} = do
  lps <- vkGetLongPollServer iLogger iGroupId iAccessKey iApiVersion
  Logger.info iLogger "VK: Getting updates..."
  let address = unpack $ lpsServer lps
  let options =
        defaults & param "act" .~ ["a_check"] & param "key" .~ [lpsKey lps] &
        param "wait" .~ [pack $ show iTimeout] &
        param "mode" .~ ["2"] &
        param "ts" .~ [pack $ show $ lpsTS lps]
  (status, body) <- Web.sendOptions address options
  case status of
    200 -> do
      Logger.info iLogger "VK: Updates received"
      updates <- either fail return (eitherDecode body :: Either String Updates)
      let events = mapMaybe updateToEvent (uUpdates updates)
      return events
    _ -> fail $ "VK: Request failed: " ++ show status

--------------------------------------------------------------------------------
vkGetLongPollServer ::
     Logger.Handle -> GroupId -> AccessKey -> ApiVersion -> IO LongPollServer
vkGetLongPollServer logger groupId accessKey apiVersion = do
  Logger.info logger "VK: Getting Long Poll Server..."
  let address = "https://api.vk.com/method/groups.getLongPollServer"
  let options =
        defaults & param "group_id" .~ [pack $ show groupId] &
        param "access_token" .~ [accessKey] &
        param "v" .~ [pack $ show apiVersion]
  (status, body) <- Web.sendOptions address options
  case status of
    200 -> do
      Logger.info logger "VK: Long Poll Server received"
      Logger.debug logger "VK: Parsing Long Poll Server"
      lps <- either fail return (eitherDecode body :: Either String LPSResponse)
      Logger.debug logger "VK: Parsed Long Poll Server"
      return (lpsResponse lps)
    _ -> fail $ "VK: Request failed: " ++ show status

--------------------------------------------------------------------------------
vkProcessEvents :: IHandle -> [Event] -> IO ()
vkProcessEvents handle [] = return ()
vkProcessEvents handle (e:events) = do
  case e of
    EventMessage {} -> processMessage handle e
    EventMedia {} -> processMedia handle e
    EventQuery {} -> processQuery handle e
  vkProcessEvents handle events

--------------------------------------------------------------------------------
processMessage :: IHandle -> Event -> IO ()
processMessage IHandle {..} EventMessage {..} = undefined
processMessage _ _ = return ()

--------------------------------------------------------------------------------
processMedia :: IHandle -> Event -> IO ()
processMedia IHandle {..} EventMedia {..} = undefined
processMedia _ _ = return ()

--------------------------------------------------------------------------------
processQuery :: IHandle -> Event -> IO ()
processQuery IHandle {..} EventQuery {..} = undefined
processQuery _ _ = return ()
