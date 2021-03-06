{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messenger.Telegram where

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text, pack, unpack)
import qualified Logger
import Messenger
import Network.HTTP.Simple

--------------------------------------------------------------------------------
new :: Config -> Logger.Handle () -> IO Handle
new config logger = do
  offset <- newIORef 0
  counters <- newIORef M.empty
  let iHandle = IHandle config logger offset counters
  return
    Handle
      { getEvents = iGetEvents iHandle
      , sendMessage = iSendMessage iHandle
      , sendMedia = iSendMedia iHandle
      , answerQuery = iAnswerQuery iHandle
      , answerHelpCommand = iAnswerHelpCommand iHandle
      , answerRepeatCommand = iAnswerRepeatCommand iHandle
      }

data IHandle =
  IHandle
    { iConfig :: Config
    , iLogger :: Logger.Handle ()
    , iOffset :: IORef Offset
    , iCounters :: IORef Counters
    }

data Config =
  Config
    { cHelpMessage :: HelpMessage
    , cRepeatMessage :: RepeatMessage
    , cRepeatCount :: RepeatCount
    , cTimeout :: Timeout
    , cToken :: Token
    }

type HelpMessage = Text

type RepeatMessage = Text

type RepeatCount = Int

type Timeout = Int

type Token = Text

type Offset = Int

type Counters = M.Map ChatId RepeatCount

--------------------------------------------------------------------------------
iGetEvents :: IHandle -> IO [Event]
-- iGetEvents = PURE                  >>= IMPURE                >>= PURE         >>= PURE
-- iGetEvents = makeGetUpdatesRequest >>= sendGetUpdatesRequest >>= parseUpdates >>= map updateToEvent
iGetEvents = undefined

--------------------------------------------------------------------------------
makeGetUpdatesRequest :: Token -> Offset -> Timeout -> Request
makeGetUpdatesRequest token offset timeout =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/getUpdates"
      host = "api.telegram.org"
   in setRequestPath path $ setRequestHost host defaultRequest

--------------------------------------------------------------------------------
type StatusCode = Int

type ResponseBody = ByteString

sendGetUpdatesRequest :: Request -> IO (StatusCode, ResponseBody)
sendGetUpdatesRequest request = do
  response <- httpLBS request
  return (getResponseStatusCode response, getResponseBody response)

--------------------------------------------------------------------------------
updateToEvent :: Update -> Event
updateToEvent Update {..}
  | isJust uMessage = messageToEvent (fromJust uMessage)
  | isJust uCallbackQuery = callbackQueryToEvent (fromJust uCallbackQuery)
  | otherwise = UnknownEvent

--------------------------------------------------------------------------------
messageToEvent :: Message -> Event
messageToEvent message =
  fromMaybe
    UnknownEvent
    (foldr1
       (<|>)
       [ eventMessage message
       , eventHelpCommand message
       , eventRepeatCommand message
       , eventSticker message
       , eventAnimation message
       , eventDocument message
       , eventPhoto message
       , eventVideo message
       , eventAudio message
       , eventVoice message
       ])

--------------------------------------------------------------------------------
eventMessage :: Message -> Maybe Event
eventMessage Message {..} =
  case mText of
    Just "/help" -> Nothing
    Just "/repeat" -> Nothing
    Just text -> Just $ EventMessage (cId mChat) text
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventHelpCommand :: Message -> Maybe Event
eventHelpCommand Message {..} =
  case mText of
    Just "/help" -> Just $ EventHelpCommand (cId mChat)
    _ -> Nothing

--------------------------------------------------------------------------------
eventRepeatCommand :: Message -> Maybe Event
eventRepeatCommand Message {..} =
  case mText of
    Just "/repeat" -> Just $ EventRepeatCommand (cId mChat)
    _ -> Nothing

--------------------------------------------------------------------------------
eventSticker :: Message -> Maybe Event
eventSticker Message {..} =
  case mSticker of
    Just sticker ->
      Just $
      EventMedia (cId mChat) (TelegramMedia Nothing MediaSticker (fId sticker))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventAnimation :: Message -> Maybe Event
eventAnimation Message {..} =
  case mAnimation of
    Just animation ->
      Just $
      EventMedia
        (cId mChat)
        (TelegramMedia mCaption MediaAnimation (fId animation))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventDocument :: Message -> Maybe Event
eventDocument Message {..} =
  case mDocument of
    Just document ->
      Just $
      EventMedia
        (cId mChat)
        (TelegramMedia mCaption MediaDocument (fId document))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventPhoto :: Message -> Maybe Event
eventPhoto Message {..} =
  case mPhoto of
    Just (photo:_) ->
      Just $
      EventMedia (cId mChat) (TelegramMedia mCaption MediaPhoto (fId photo))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventVideo :: Message -> Maybe Event
eventVideo Message {..} =
  case mVideo of
    Just video ->
      Just $
      EventMedia (cId mChat) (TelegramMedia mCaption MediaVideo (fId video))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventAudio :: Message -> Maybe Event
eventAudio Message {..} =
  case mAudio of
    Just audio ->
      Just $
      EventMedia (cId mChat) (TelegramMedia mCaption MediaAudio (fId audio))
    Nothing -> Nothing

--------------------------------------------------------------------------------
eventVoice :: Message -> Maybe Event
eventVoice Message {..} =
  case mVoice of
    Just voice ->
      Just $
      EventMedia (cId mChat) (TelegramMedia mCaption MediaVoice (fId voice))
    Nothing -> Nothing

--------------------------------------------------------------------------------
callbackQueryToEvent :: CallbackQuery -> Event
callbackQueryToEvent CallbackQuery {..}
  | isJust cqData = EventQuery (cId cqFrom) cqId (fromJust cqData)
  | otherwise = UnknownEvent

--------------------------------------------------------------------------------
newtype Updates =
  Updates
    { uResult :: [Update]
    }
  deriving (Show)

instance A.FromJSON Updates where
  parseJSON =
    A.withObject "Messenger.Telegram.Updates" $ \o ->
      Updates <$> o A..: "result"

data Update =
  Update
    { uUpdateId :: Int
    , uMessage :: Maybe Message
    , uCallbackQuery :: Maybe CallbackQuery
    }
  deriving (Show)

instance A.FromJSON Update where
  parseJSON =
    A.withObject "Messenger.Telegram.Updates" $ \o ->
      Update <$> o A..: "update_id" <*> o A..: "message" <*>
      o A..: "callback_query"

data Message =
  Message
    { mChat :: Chat
    , mText :: Maybe Text
    , mCaption :: Maybe Caption
    , mSticker :: Maybe File
    , mAnimation :: Maybe File
    , mDocument :: Maybe File
    , mPhoto :: Maybe [File]
    , mVideo :: Maybe File
    , mAudio :: Maybe File
    , mVoice :: Maybe File
    }
  deriving (Show)

instance A.FromJSON Message where
  parseJSON =
    A.withObject "Messenger.Telegram.Message" $ \o ->
      Message <$> o A..: "chat" <*> o A..:? "text" <*> o A..:? "caption" <*>
      o A..:? "sticker" <*>
      o A..:? "animation" <*>
      o A..:? "document" <*>
      o A..:? "photo" <*>
      o A..:? "video" <*>
      o A..:? "audio" <*>
      o A..:? "voice"

data CallbackQuery =
  CallbackQuery
    { cqId :: QueryId
    , cqFrom :: Chat
    , cqData :: Maybe QueryData
    }
  deriving (Show)

instance A.FromJSON CallbackQuery where
  parseJSON =
    A.withObject "Messenger.Telegram.CallbackQuery" $ \o ->
      CallbackQuery <$> o A..: "id" <*> o A..: "from" <*> o A..: "data"

newtype Chat =
  Chat
    { cId :: ChatId
    }
  deriving (Show)

instance A.FromJSON Chat where
  parseJSON =
    A.withObject "Meesenger.Telegram.Chat" $ \o -> Chat <$> o A..: "id"

newtype File =
  File
    { fId :: FileId
    }
  deriving (Show)

instance A.FromJSON File where
  parseJSON =
    A.withObject "Messenger.Telegram.File" $ \o -> File <$> o A..: "file_id"

--------------------------------------------------------------------------------
iSendMessage :: IHandle -> ChatId -> Text -> IO ()
iSendMessage = undefined

--------------------------------------------------------------------------------
iSendMedia :: IHandle -> ChatId -> Media -> IO ()
iSendMedia = undefined

--------------------------------------------------------------------------------
iAnswerQuery :: IHandle -> QueryId -> QueryAnswer -> IO ()
iAnswerQuery = undefined

--------------------------------------------------------------------------------
iAnswerHelpCommand :: IHandle -> ChatId -> IO ()
iAnswerHelpCommand = undefined

--------------------------------------------------------------------------------
iAnswerRepeatCommand :: IHandle -> ChatId -> IO ()
iAnswerRepeatCommand = undefined
