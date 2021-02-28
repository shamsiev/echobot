{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK where

import Bot
import Bot.VK.Internal
import Control.Lens ((&), (.~))
import Control.Monad (replicateM_)
import Data.Aeson
  ( ToJSON(..)
  , Value
  , (.=)
  , eitherDecode
  , encode
  , object
  , toJSON
  )
import Data.ByteString.Lazy.Internal (unpackChars)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON(parseJSON), (.:), withObject)
import qualified Logger
import Network.Wreq
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
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
new :: Logger.Handle -> Config -> IConfig -> IO Handle
new logger config iConfig = do
  counters <- newIORef M.empty
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
    EventMessage {..} -> processMessage handle e
    EventMedia {..} -> print "media event"
    EventQuery {..} -> print "media event"
  vkProcessEvents handle events

--------------------------------------------------------------------------------
processMessage :: IHandle -> Event -> IO ()
processMessage IHandle {..} EventMessage {..} = do
  Logger.info iLogger $ "VK: Processing message for: " <> pack (show eChatId)
  counters <- readIORef iCounters
  let counter = fromMaybe iDefaultRepeat $ M.lookup eChatId counters
  Logger.debug iLogger $ "VK: Current repeat count: " <> pack (show counter)
  let address = "https://api.vk.com/method/messages.send"
  case eMessage of
    "/help" -> do
      let options =
            defaults & param "user_id" .~ [pack $ show eChatId] &
            param "message" .~ [iHelpMessage] &
            param "random_id" .~ ["0"] &
            param "access_token" .~ [iAccessKey] &
            param "v" .~ [iApiVersion]
      (code, _) <- Web.sendOptions address options
      case code of
        200 -> Logger.info iLogger "VK: Sent /help message"
        _ ->
          Logger.error iLogger $
          "VK: Sending /help message failed: " <> pack (show code)
    "/repeat" -> do
      let options =
            defaults & param "user_id" .~ [pack $ show eChatId] &
            param "message" .~ [iRepeatMessage] &
            param "keyboard" .~ [pack $ unpackChars $ encode keyboard] &
            param "random_id" .~ ["0"] &
            param "access_token" .~ [iAccessKey] &
            param "v" .~ [iApiVersion]
      (code, body) <- Web.sendOptions address options
      print body
      case code of
        200 -> Logger.info iLogger "VK: Sent /repeat message"
        _ ->
          Logger.error iLogger $
          "VK: Sending /repeat message failed: " <> pack (show code)
    _ -> do
      let options =
            defaults & param "user_id" .~ [pack $ show eChatId] &
            param "message" .~ [eMessage] &
            param "random_id" .~ ["0"] &
            param "access_token" .~ [iAccessKey] &
            param "v" .~ [iApiVersion]
      (code, _) <- Web.sendOptions address options
      case code of
        200 -> Logger.info iLogger "VK: Sent message"
        _ ->
          Logger.error iLogger $
          "VK: Sending message failed: " <> pack (show code)
processMessage _ _ = fail "VK: Used processMessage in a wrong place"

keyboard :: Keyboard
keyboard = Keyboard [buttons]

newtype Keyboard =
  Keyboard
    { kButtons :: [[Button]]
    }
  deriving (Show)

instance ToJSON Keyboard where
  toJSON Keyboard {..} = object ["buttons" .= kButtons]

buttons :: [Button]
buttons =
  [ Button
      { bAction =
          ButtonAction {baType = "callback", baLabel = "1", baPayload = "1"}
      , bColor = "primary"
      }
  , Button
      { bAction =
          ButtonAction {baType = "callback", baLabel = "2", baPayload = "2"}
      , bColor = "primary"
      }
  , Button
      { bAction =
          ButtonAction {baType = "callback", baLabel = "3", baPayload = "3"}
      , bColor = "primary"
      }
  , Button
      { bAction =
          ButtonAction {baType = "callback", baLabel = "4", baPayload = "4"}
      , bColor = "primary"
      }
  , Button
      { bAction =
          ButtonAction {baType = "callback", baLabel = "5", baPayload = "5"}
      , bColor = "primary"
      }
  ]

data Button =
  Button
    { bAction :: ButtonAction
    , bColor :: Text
    }
  deriving (Show)

instance ToJSON Button where
  toJSON Button {..} = object ["action" .= bAction, "color" .= bColor]

data ButtonAction =
  ButtonAction
    { baType :: Text
    , baLabel :: Text
    , baPayload :: Text
    }
  deriving (Show)

instance ToJSON ButtonAction where
  toJSON ButtonAction {..} =
    object ["type" .= baType, "label" .= baLabel, "payload" .= baPayload]
