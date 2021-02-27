{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram where

import Bot (ChatId, Event(..), Media(..))
import Bot.Telegram.Internal
  ( Update(uUpdateId)
  , Updates(uResult)
  , updateToEvent
  )
import Control.Monad (replicateM_)
import Data.Aeson
  ( KeyValue((.=))
  , ToJSON(..)
  , Value
  , eitherDecode
  , object
  , toJSON
  )
import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text, pack, unpack)
import qualified Logger
import qualified Web

--------------------------------------------------------------------------------
type Token = Text

type Timeout = Int

type Offset = Int

type Counter = Int

type Counters = M.Map ChatId Counter

--------------------------------------------------------------------------------
data IHandle =
  IHandle
    { iToken :: Token
    , iHelpMessage :: Text
    , iRepeatMessage :: Text
    , iDefaultRepeat :: Counter
    , iTimeout :: Timeout
    , iOffset :: IORef Offset
    , iCounters :: IORef Counters
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
tgGetEvents :: IHandle -> IO [Event]
tgGetEvents IHandle {..} = do
  offset <- readIORef iOffset
  Logger.debug iLogger $
    "Telegram: Current update offset: " <> pack (show offset)
  Logger.info iLogger "Telegram: Getting updates..."
  let address = "https://api.telegram.org/bot" ++ unpack iToken ++ "/getUpdates"
  let json =
        object
          ["offset" .= pack (show offset), "timeout" .= pack (show iTimeout)]
  (status, body) <- Web.sendJSON address json
  case status of
    200 -> do
      Logger.info iLogger "Telegram: Updates received"
      Logger.debug iLogger "Telegram: Parsing updates..."
      let updates = eitherDecode body :: Either String Updates
      case updates of
        Left err -> fail err
        Right results -> do
          let events = mapMaybe updateToEvent (uResult results)
          Logger.debug iLogger "Telegram: Setting update offset..."
          let newOffset =
                maximum $ offset : map ((+ 1) . uUpdateId) (uResult results)
          Logger.debug iLogger $
            "Telegram: New update offset: " <> pack (show newOffset)
          writeIORef iOffset newOffset
          Logger.debug iLogger $
            "Telegram: Current events: " <> pack (show events)
          return events
    code -> fail $ "Telegram: Request failed: " ++ show code

--------------------------------------------------------------------------------
tgProcessEvents :: IHandle -> [Event] -> IO ()
tgProcessEvents handle [] = return ()
tgProcessEvents handle (e:events) = do
  case e of
    EventMessage {} -> processMessage handle e
    EventMedia {} -> processMedia handle e
    EventQuery {} -> processQuery handle e
  tgProcessEvents handle events

--------------------------------------------------------------------------------
processMessage :: IHandle -> Event -> IO ()
processMessage IHandle {..} EventMessage {..} = do
  Logger.info iLogger $
    "Telegram: Processing message for: " <> pack (show eChatId)
  counters <- readIORef iCounters
  let counter = fromMaybe iDefaultRepeat $ M.lookup eChatId counters
  Logger.debug iLogger $
    "Telegram: Current repeat count: " <> pack (show counter)
  let address =
        "https://api.telegram.org/bot" ++ unpack iToken ++ "/sendMessage"
  case eMessage of
    "/help" -> do
      let json = object ["chat_id" .= eChatId, "text" .= iHelpMessage]
      (code, _) <- Web.sendJSON address json
      case code of
        200 -> Logger.info iLogger "Telegram: Sent /help message"
        _ ->
          Logger.error iLogger $
          "Telegram: Sending /help message failed: " <> pack (show code)
    "/repeat" -> do
      let json =
            object
              [ "chat_id" .= eChatId
              , "text" .= iRepeatMessage
              , "reply_markup" .= object ["inline_keyboard" .= keyboard]
              ]
      (code, _) <- Web.sendJSON address json
      case code of
        200 -> Logger.info iLogger "Telegram: Sent /repeat meesage"
        _ ->
          Logger.error iLogger $
          "Telegram: Sending /repeat message failed: " <> pack (show code)
    _ -> do
      let json = object ["chat_id" .= eChatId, "text" .= eMessage]
      replicateM_ counter $ do
        (code, _) <- Web.sendJSON address json
        case code of
          200 -> Logger.info iLogger "Telegram: Sent message"
          _ ->
            Logger.error iLogger $
            "Telegram: Sending message failed: " <> pack (show code)
processMessage _ _ = fail "Telegram: processMessage used in a wrong place"

keyboard :: Value
keyboard =
  toJSON
    [ [Button "1" "1"]
    , [Button "2" "2"]
    , [Button "3" "3"]
    , [Button "4" "4"]
    , [Button "5" "5"]
    ]

data Button =
  Button
    { bText :: Text
    , bData :: Text
    }
  deriving (Show)

instance ToJSON Button where
  toJSON Button {..} = object ["text" .= bText, "callback_data" .= bData]

--------------------------------------------------------------------------------
processMedia :: IHandle -> Event -> IO ()
processMedia IHandle {..} EventMedia {..} = do
  Logger.info iLogger $
    "Telegram: Processing media for: " <> pack (show eChatId)
  counters <- readIORef iCounters
  let counter = fromMaybe iDefaultRepeat $ M.lookup eChatId counters
  Logger.debug iLogger $
    "Telegram: Current repeat count: " <> pack (show counter)
  let address =
        "https://api.telegram.org/bot" ++
        unpack iToken ++ methodNameFromMedia eMedia
  let json = jsonFromMedia eChatId eMedia
  replicateM_ counter $ do
    (code, _) <- Web.sendJSON address json
    case code of
      200 -> Logger.info iLogger "Telegram: Sent media"
      _ ->
        Logger.error iLogger $
        "Telegram: Sending message failed: " <> pack (show code)
processMedia _ _ = fail "Telegram: processMedia used in a wrong place"

--------------------------------------------------------------------------------
methodNameFromMedia :: Media -> String
methodNameFromMedia media =
  case media of
    MediaSticker _ -> "/sendSticker"
    MediaAnimation _ _ -> "/sendAnimation"
    MediaDocument _ _ -> "/sendDocument"
    MediaPhoto _ _ -> "/sendPhoto"
    MediaVideo _ _ -> "/sendVideo"
    MediaAudio _ _ -> "/sendAudio"
    MediaVoice _ _ -> "/sendVoice"

--------------------------------------------------------------------------------
jsonFromMedia :: ChatId -> Media -> Value
jsonFromMedia chatId media =
  case media of
    MediaSticker file -> object ["chat_id" .= chatId, "sticker" .= file]
    MediaAnimation file caption ->
      object ["chat_id" .= chatId, "animation" .= file, "caption" .= caption]
    MediaDocument file caption ->
      object ["chat_id" .= chatId, "document" .= file, "caption" .= caption]
    MediaPhoto file caption ->
      object ["chat_id" .= chatId, "photo" .= file, "caption" .= caption]
    MediaVideo file caption ->
      object ["chat_id" .= chatId, "video" .= file, "caption" .= caption]
    MediaAudio file caption ->
      object ["chat_id" .= chatId, "audio" .= file, "caption" .= caption]
    MediaVoice file caption ->
      object ["chat_id" .= chatId, "voice" .= file, "caption" .= caption]

--------------------------------------------------------------------------------
processQuery :: IHandle -> Event -> IO ()
processQuery IHandle {..} EventQuery {..} = do
  Logger.info iLogger $ "Telegram: answering callback_query " <> eQueryId
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Telegram: Current counters: " <> pack (show counters)
  let address =
        "https://api.telegram.org/bot" ++
        unpack iToken ++ "/answerCallbackQuery"
  let json =
        object
          [ "callback_query_id" .= eQueryId
          , "text" .= ("Updated repeat count to: " <> eUserdata)
          ]
  Logger.info iLogger $
    "Telegram: Setting repeat count: " <> pack (show eChatId) <> " : " <>
    eUserdata
  (code, _) <- Web.sendJSON address json
  case code of
    200 -> do
      Logger.info iLogger "Telegram: Set repeat count"
      let newCounter = (read (unpack eUserdata) :: Counter)
      Logger.debug iLogger $
        "Telegram: Set counter for " <> pack (show eChatId) <> " to " <>
        eUserdata
      let updatedCounters = M.insert eChatId newCounter counters
      writeIORef iCounters updatedCounters
    _ ->
      Logger.error iLogger $
      "Telegram: Failed to answer callback_query: " <> pack (show code)
processQuery _ _ = fail "Telegram: processQuery used in a wrong place"
