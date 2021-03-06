{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messenger.Telegram where

import Control.Applicative
import Control.Monad (replicateM_, when)
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
import Prelude hiding (error)

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

--------------------------------------------------------------------------------
data IHandle =
  IHandle
    { iConfig :: Config
    , iLogger :: Logger.Handle ()
    , iOffset :: IORef Offset
    , iCounters :: IORef Counters
    }

--------------------------------------------------------------------------------
data Config =
  Config
    { cHelpMessage :: HelpMessage
    , cRepeatMessage :: RepeatMessage
    , cRepeatCount :: RepeatCount
    , cTimeout :: Timeout
    , cToken :: Token
    }

--------------------------------------------------------------------------------
type HelpMessage = Text

type RepeatMessage = Text

type RepeatCount = Int

type Timeout = Int

type Token = Text

type Offset = Int

type Counters = M.Map ChatId RepeatCount

--------------------------------------------------------------------------------
iGetEvents :: IHandle -> IO [Event]
iGetEvents IHandle {..} = do
  Logger.info iLogger "Gettings events..."
  offset <- readIORef iOffset
  Logger.debug iLogger $ "Current offset: " <> pack (show offset)
  let request = makeGetUpdatesRequest (cToken iConfig) offset (cTimeout iConfig)
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> do
      Logger.info iLogger "Received updates"
      updates <-
        either
          fail
          (return . uResult)
          (A.eitherDecode (getResponseBody response) :: Either String Updates)
      Logger.debug iLogger $ "Parsed updates: " <> pack (show updates)
      let events = map updateToEvent updates
      let newOffset = maximum $ offset : map ((+ 1) . uUpdateId) updates
      writeIORef iOffset newOffset
      Logger.debug iLogger $ "Current events: " <> pack (show events)
      return events
    code -> do
      Logger.error iLogger $
        "Failed on gettings updates with code: " <> pack (show code)
      fail "Failed getting events"

--------------------------------------------------------------------------------
makeGetUpdatesRequest :: Token -> Offset -> Timeout -> Request
makeGetUpdatesRequest token offset timeout =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/getUpdates"
      host = "api.telegram.org"
      requestBody = A.object ["offset" A..= offset, "timeout" A..= timeout]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
newtype Chat =
  Chat
    { cId :: ChatId
    }
  deriving (Show)

instance A.FromJSON Chat where
  parseJSON =
    A.withObject "Meesenger.Telegram.Chat" $ \o -> Chat <$> o A..: "id"

--------------------------------------------------------------------------------
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
iSendMessage IHandle {..} chatId text = do
  Logger.info iLogger "Sending message..."
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Current counters: " <> pack (show counters)
  let repeatCount = M.findWithDefault (cRepeatCount iConfig) chatId counters
  Logger.debug iLogger $
    "Counter chosen for sendMessage: " <> pack (show repeatCount)
  let request = makeSendMessageRequest (cToken iConfig) chatId text
  replicateM_ repeatCount $ do
    response <- httpLBS request
    case getResponseStatusCode response of
      200 -> Logger.info iLogger "Successfully sent message"
      code ->
        Logger.warning iLogger $ "Sent message with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeSendMessageRequest :: Token -> ChatId -> Text -> Request
makeSendMessageRequest token chatId text =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendMessage"
      host = "api.telegram.org"
      requestBody = A.object ["chat_id" A..= chatId, "text" A..= text]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
iSendMedia :: IHandle -> ChatId -> Media -> IO ()
iSendMedia IHandle {..} chatId media = do
  Logger.info iLogger "Sending media..."
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Current counters: " <> pack (show counters)
  let repeatCount = M.findWithDefault (cRepeatCount iConfig) chatId counters
  Logger.debug iLogger $
    "Counter chosen to send media: " <> pack (show repeatCount)
  request <- either fail return $ makeMediaRequest (cToken iConfig) chatId media
  replicateM_ repeatCount $ do
    response <- httpLBS request
    case getResponseStatusCode response of
      200 -> Logger.info iLogger "Successfully sent media"
      code ->
        Logger.warning iLogger $ "Sent media with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeMediaRequest :: Token -> ChatId -> Media -> Either String Request
makeMediaRequest token chatId (TelegramMedia _ MediaSticker fileId) =
  Right $ makeSendStickerRequest token chatId fileId
makeMediaRequest token chatId (TelegramMedia caption MediaVoice fileId) =
  Right $ makeSendVoiceRequest token chatId caption fileId
makeMediaRequest token chatId (TelegramMedia caption MediaAnimation fileId) =
  Right $ makeSendAnimationRequest token chatId caption fileId
makeMediaRequest token chatId (TelegramMedia caption MediaPhoto fileId) =
  Right $ makeSendPhotoRequest token chatId caption fileId
makeMediaRequest token chatId (TelegramMedia caption MediaVideo fileId) =
  Right $ makeSendVideoRequest token chatId caption fileId
makeMediaRequest token chatId (TelegramMedia caption MediaAudio fileId) =
  Right $ makeSendAudioRequest token chatId caption fileId
makeMediaRequest token chatId (TelegramMedia caption MediaDocument fileId) =
  Right $ makeSendDocumentRequest token chatId caption fileId
makeMediaRequest _ _ _ = Left "Invalid media"

--------------------------------------------------------------------------------
makeSendStickerRequest :: Token -> ChatId -> FileId -> Request
makeSendStickerRequest token chatId fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendSticker"
      host = "api.telegram.org"
      requestBody = A.object ["chat_id" A..= chatId, "sticker" A..= fileId]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendVoiceRequest :: Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendVoiceRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendVoice"
      host = "api.telegram.org"
      requestBody =
        A.object
          ["chat_id" A..= chatId, "voice" A..= fileId, "caption" A..= caption]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendAnimationRequest ::
     Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendAnimationRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendAnimation"
      host = "api.telegram.org"
      requestBody =
        A.object
          [ "chat_id" A..= chatId
          , "animation" A..= fileId
          , "caption" A..= caption
          ]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendPhotoRequest :: Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendPhotoRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendPhoto"
      host = "api.telegram.org"
      requestBody =
        A.object
          ["chat_id" A..= chatId, "photo" A..= fileId, "caption" A..= caption]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendVideoRequest :: Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendVideoRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendVideo"
      host = "api.telegram.org"
      requestBody =
        A.object
          ["chat_id" A..= chatId, "video" A..= fileId, "caption" A..= caption]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendAudioRequest :: Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendAudioRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendAudio"
      host = "api.telegram.org"
      requestBody =
        A.object
          ["chat_id" A..= chatId, "audio" A..= fileId, "caption" A..= caption]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
makeSendDocumentRequest :: Token -> ChatId -> Maybe Caption -> FileId -> Request
makeSendDocumentRequest token chatId caption fileId =
  let path = BSC.pack $ "/bot" ++ unpack token ++ "/sendDocument"
      host = "api.telegram.org"
      requestBody =
        A.object
          [ "chat_id" A..= chatId
          , "document" A..= fileId
          , "caption" A..= caption
          ]
   in setRequestPath path $
      setRequestHost host $ setRequestBodyJSON requestBody defaultRequest

--------------------------------------------------------------------------------
iAnswerQuery :: IHandle -> QueryId -> QueryAnswer -> IO ()
iAnswerQuery = undefined

--------------------------------------------------------------------------------
iAnswerHelpCommand :: IHandle -> ChatId -> IO ()
iAnswerHelpCommand = undefined

--------------------------------------------------------------------------------
iAnswerRepeatCommand :: IHandle -> ChatId -> IO ()
iAnswerRepeatCommand = undefined
