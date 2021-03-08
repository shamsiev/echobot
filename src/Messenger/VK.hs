{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messenger.VK where

import Control.Applicative (Alternative((<|>)))
import Control.Monad (replicateM_)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import Data.ByteString.Lazy.Internal (unpackChars)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Logger
import Messenger
  ( ChatId
  , Event(..)
  , Handle(..)
  , Media(VKMedia)
  , MediaType(MediaAudio, MediaDocument, MediaPhoto, MediaVideo)
  , QueryData
  , QueryId
  , VKMediaFile(..)
  )
import Network.HTTP.Simple
  ( Request
  , defaultRequest
  , getResponseBody
  , getResponseStatusCode
  , httpLBS
  , setRequestHost
  , setRequestPath
  , setRequestQueryString
  )

--------------------------------------------------------------------------------
new :: Config -> Logger.Handle () -> IO Handle
new config logger = do
  counters <- newIORef M.empty
  let iHandle = IHandle config logger counters
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
    , cGroupId :: GroupId
    , cApiVersion :: ApiVersion
    }

--------------------------------------------------------------------------------
type RepeatCount = Int

type Counters = M.Map ChatId RepeatCount

type HelpMessage = Text

type RepeatMessage = Text

type Timeout = Int

type Token = Text

type GroupId = Text

type ApiVersion = Text

type PollKey = Text

type PollTS = Text

--------------------------------------------------------------------------------
iGetEvents :: IHandle -> IO [Event]
iGetEvents IHandle {..} = do
  Logger.info iLogger "Getting events..."
  let lpRequest =
        makeGetLongPollServerRequest
          (cToken iConfig)
          (cGroupId iConfig)
          (cApiVersion iConfig)
  Logger.debug iLogger "Establishing long poll connection..."
  lpResponse <- httpLBS lpRequest
  Logger.debug iLogger "Parsing Long Poll Server..."
  lps <-
    either
      fail
      (return . lpsResponse)
      (A.eitherDecode (getResponseBody lpResponse) :: Either String LongPollServer)
  Logger.debug iLogger $ "Current Long Poll Server: " <> pack (show lps)
  let uRequest =
        makeGetUpdatesRequest
          (cGroupId iConfig)
          (lpsKey lps)
          (lpsTS lps)
          (cTimeout iConfig)
  uResponse <- httpLBS uRequest
  Logger.debug iLogger "Parsing updates..."
  updates <-
    either
      fail
      (return . uUpdates)
      (A.eitherDecode (getResponseBody uResponse) :: Either String Updates)
  Logger.debug iLogger $ "Current updates: " <> pack (show updates)
  Logger.debug iLogger "Parsing events..."
  let events = map updateToEvent updates
  Logger.debug iLogger $ "Current events: " <> pack (show events)
  return events

--------------------------------------------------------------------------------
makeGetLongPollServerRequest :: Token -> GroupId -> ApiVersion -> Request
makeGetLongPollServerRequest token groupId apiVersion = do
  let path = BSC.pack "/method/groups.getLongPollServer"
      host = "api.vk.com"
      queryString =
        [ ("group_id", Just $ encodeUtf8 groupId)
        , ("access_token", Just $ encodeUtf8 token)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
newtype LongPollServer =
  LongPollServer
    { lpsResponse :: LPSResponse
    }
  deriving (Show)

instance A.FromJSON LongPollServer where
  parseJSON =
    A.withObject "Messenger.VK.LongPollServer" $ \o ->
      LongPollServer <$> o A..: "response"

--------------------------------------------------------------------------------
data LPSResponse =
  LPSResponse
    { lpsKey :: Text
    , lpsServer :: Text
    , lpsTS :: Text
    }
  deriving (Show)

instance A.FromJSON LPSResponse where
  parseJSON =
    A.withObject "Messenger.VK.LPSResponse" $ \o ->
      LPSResponse <$> o A..: "key" <*> o A..: "server" <*> o A..: "ts"

--------------------------------------------------------------------------------
makeGetUpdatesRequest :: GroupId -> PollKey -> PollTS -> Timeout -> Request
makeGetUpdatesRequest groupId key ts timeout =
  let path = encodeUtf8 $ "/wh" <> groupId
      host = "lp.vk.com"
      queryString =
        [ ("act", Just "a_check")
        , ("key", Just $ encodeUtf8 key)
        , ("wait", Just $ BSC.pack $ show timeout)
        , ("mode", Just "2")
        , ("ts", Just $ encodeUtf8 ts)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
newtype Updates =
  Updates
    { uUpdates :: [Update]
    }
  deriving (Show)

instance A.FromJSON Updates where
  parseJSON =
    A.withObject "Messenger.VK.Updates" $ \o -> Updates <$> o A..: "updates"

--------------------------------------------------------------------------------
newtype Update =
  Update
    { uObject :: UObject
    }
  deriving (Show)

instance A.FromJSON Update where
  parseJSON =
    A.withObject "Messenger.VK.Update" $ \o -> Update <$> o A..: "object"

--------------------------------------------------------------------------------
newtype UObject =
  UObject
    { uMessage :: Maybe Message
    }
  deriving (Show)

instance A.FromJSON UObject where
  parseJSON =
    A.withObject "FromJSON Bot.VK.Internal.UObject" $ \o ->
      UObject <$> o A..:? "message"

--------------------------------------------------------------------------------
data Message =
  Message
    { mFromId :: Int
    , mText :: Maybe Text
    , mAttachments :: [Attachment]
    , mPayload :: Maybe Text
    , mEventId :: Maybe Text
    }
  deriving (Show)

instance A.FromJSON Message where
  parseJSON =
    A.withObject "FromJSON Bot.VK.Internal.Message" $ \o ->
      Message <$> o A..: "from_id" <*> o A..:? "text" <*> o A..: "attachments" <*>
      o A..:? "payload" <*>
      o A..:? "event_id"

--------------------------------------------------------------------------------
data Attachment =
  Attachment
    { aPhoto :: Maybe File
    , aAudio :: Maybe File
    , aSticker :: Maybe Sticker
    , aDocument :: Maybe File
    , aVideo :: Maybe File
    }
  deriving (Show)

instance A.FromJSON Attachment where
  parseJSON =
    A.withObject "FromJSON Bot.VK.Internal.Attachment" $ \o ->
      Attachment <$> o A..:? "photo" <*> o A..:? "audio" <*> o A..:? "sticker" <*>
      o A..:? "doc" <*>
      o A..:? "video"

--------------------------------------------------------------------------------
data File =
  File
    { fAccessKey :: Maybe Text
    , fId :: Int
    , fOwnerId :: Int
    }
  deriving (Show)

instance A.FromJSON File where
  parseJSON =
    A.withObject "FromJSON Bot.VK.Internal.File" $ \o ->
      File <$> o A..:? "access_key" <*> o A..: "id" <*> o A..: "owner_id"

--------------------------------------------------------------------------------
newtype Sticker =
  Sticker
    { sId :: Int
    }
  deriving (Show)

instance A.FromJSON Sticker where
  parseJSON =
    A.withObject "FromJSON Bot.VK.Internal.Sticker" $ \o ->
      Sticker <$> o A..: "sticker_id"

--------------------------------------------------------------------------------
updateToEvent :: Update -> Event
updateToEvent Update {..}
  | isJust $ uMessage uObject = messageToEvent (fromJust $ uMessage uObject)
  | otherwise = UnknownEvent

--------------------------------------------------------------------------------
messageToEvent :: Message -> Event
messageToEvent message =
  fromMaybe
    UnknownEvent
    (foldr1
       (<|>)
       [ eventQuery message
       , eventMessage message
       , eventHelpCommand message
       , eventRepeatCommand message
       , eventMedia message
       ])

--------------------------------------------------------------------------------
eventMessage :: Message -> Maybe Event
eventMessage Message {..} =
  case mText of
    Nothing -> Nothing
    Just "/help" -> Nothing
    Just "/repeat" -> Nothing
    Just text ->
      case mAttachments of
        [] -> Just $ EventMessage mFromId text
        _ -> Nothing

--------------------------------------------------------------------------------
eventHelpCommand :: Message -> Maybe Event
eventHelpCommand Message {..} =
  case mText of
    Just "/help" -> Just $ EventHelpCommand mFromId
    _ -> Nothing

--------------------------------------------------------------------------------
eventRepeatCommand :: Message -> Maybe Event
eventRepeatCommand Message {..} =
  case mText of
    Just "/repeat" -> Just $ EventRepeatCommand mFromId
    _ -> Nothing

--------------------------------------------------------------------------------
eventMedia :: Message -> Maybe Event
eventMedia Message {..} =
  case mAttachments of
    [] -> Nothing
    _ ->
      Just $
      EventMedia
        mFromId
        (VKMedia mText (mapMaybe attachmentToMedia mAttachments))

--------------------------------------------------------------------------------
attachmentToMedia :: Attachment -> Maybe VKMediaFile
attachmentToMedia Attachment {..}
  | isJust aPhoto =
    Just $
    VKMediaFile MediaPhoto (fOwnerId $ fromJust aPhoto) (fId $ fromJust aPhoto)
  | isJust aAudio =
    Just $
    VKMediaFile MediaAudio (fOwnerId $ fromJust aAudio) (fId $ fromJust aAudio)
  | isJust aSticker = Just $ VKSticker (sId $ fromJust aSticker)
  | isJust aDocument =
    Just $
    VKMediaFile
      MediaDocument
      (fOwnerId $ fromJust aDocument)
      (fId $ fromJust aDocument)
  | isJust aVideo =
    Just $
    VKMediaFile MediaVideo (fOwnerId $ fromJust aVideo) (fId $ fromJust aVideo)
  | otherwise = Nothing

--------------------------------------------------------------------------------
eventQuery :: Message -> Maybe Event
eventQuery Message {..}
  | isJust mPayload && isJust mText && isJust mEventId =
    Just $ EventQuery mFromId (fromJust mEventId) (fromJust mText)
  | otherwise = Nothing

--------------------------------------------------------------------------------
iSendMessage :: IHandle -> ChatId -> Text -> IO ()
iSendMessage IHandle {..} chatId text = do
  Logger.info iLogger "Sending message..."
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Current counters: " <> pack (show counters)
  let repeatCount = M.findWithDefault (cRepeatCount iConfig) chatId counters
  Logger.debug iLogger $
    "Counter chosen for sending message: " <> pack (show repeatCount)
  let request =
        makeSendMessageRequest
          (cToken iConfig)
          (cApiVersion iConfig)
          chatId
          text
  replicateM_ repeatCount $ do
    response <- httpLBS request
    case getResponseStatusCode response of
      200 -> Logger.info iLogger "Successfully sent message"
      code ->
        Logger.warning iLogger $ "Sent message with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeSendMessageRequest :: Token -> ApiVersion -> ChatId -> Text -> Request
makeSendMessageRequest token apiVersion chatId text =
  let path = "/method/messages.send"
      host = "api.vk.com"
      queryString =
        [ ("user_id", Just $ BSC.pack $ show chatId)
        , ("message", Just $ encodeUtf8 text)
        , ("random_id", Just "0")
        , ("access_token", Just $ encodeUtf8 token)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
iSendMedia :: IHandle -> ChatId -> Media -> IO ()
iSendMedia IHandle {..} chatId (VKMedia text mediaFiles) = do
  Logger.info iLogger "Sending media..."
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Current counters: " <> pack (show counters)
  let repeatCount = M.findWithDefault (cRepeatCount iConfig) chatId counters
  Logger.debug iLogger $
    "Counter chosen for sending message: " <> pack (show repeatCount)
  let request =
        makeSendMediaRequest
          (cToken iConfig)
          (cApiVersion iConfig)
          chatId
          text
          mediaFiles
  replicateM_ repeatCount $ do
    response <- httpLBS request
    case getResponseStatusCode response of
      200 -> Logger.info iLogger "Successfully sent media"
      code ->
        Logger.warning iLogger $ "Sent media with code: " <> pack (show code)
iSendMedia _ _ _ = fail "Unable to send media: wrong format"

--------------------------------------------------------------------------------
makeSendMediaRequest ::
     Token -> ApiVersion -> ChatId -> Maybe Text -> [VKMediaFile] -> Request
makeSendMediaRequest token apiVersion chatId text mediaFiles =
  let path = "/method/messages.send"
      host = "api.vk.com"
      queryString =
        [ ("user_id", Just $ BSC.pack $ show chatId)
        , ("message", encodeUtf8 <$> text)
        , ("sticker_id", findSticker mediaFiles)
        , ("random_id", Just "0")
        , ("access_token", Just $ encodeUtf8 token)
        , ( "attachment"
          , Just $ BSC.intercalate "," $ mapMaybe mediaFileToBSC mediaFiles)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
findSticker :: [VKMediaFile] -> Maybe BSC.ByteString
findSticker [] = Nothing
findSticker ((VKSticker stickerId):_) = Just $ BSC.pack $ show stickerId
findSticker (_:mediaFiles) = findSticker mediaFiles

--------------------------------------------------------------------------------
mediaFileToBSC :: VKMediaFile -> Maybe BSC.ByteString
mediaFileToBSC (VKMediaFile MediaPhoto ownerId mediaId) =
  Just $ BSC.pack $ "photo" ++ show ownerId ++ "_" ++ show mediaId
mediaFileToBSC (VKMediaFile MediaVideo ownerId mediaId) =
  Just $ BSC.pack $ "video" ++ show ownerId ++ "_" ++ show mediaId
mediaFileToBSC (VKMediaFile MediaAudio ownerId mediaId) =
  Just $ BSC.pack $ "audio" ++ show ownerId ++ "_" ++ show mediaId
mediaFileToBSC (VKMediaFile MediaDocument ownerId mediaId) =
  Just $ BSC.pack $ "doc" ++ show ownerId ++ "_" ++ show mediaId
mediaFileToBSC _ = Nothing

--------------------------------------------------------------------------------
iAnswerQuery :: IHandle -> ChatId -> QueryId -> QueryData -> IO ()
iAnswerQuery IHandle {..} chatId queryId queryData = do
  Logger.info iLogger "Answering callback query..."
  counters <- readIORef iCounters
  Logger.debug iLogger $ "Current counters: " <> pack (show counters)
  let updatedCounters =
        M.insert chatId (read (unpack queryData) :: Int) counters
  Logger.debug iLogger $ "Updated counters: " <> pack (show updatedCounters)
  writeIORef iCounters updatedCounters
  let request =
        makeAnswerCallbackQueryRequest
          (cToken iConfig)
          (cApiVersion iConfig)
          chatId
          queryId
          queryData
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> Logger.info iLogger "Successfully answered callback query"
    code ->
      Logger.warning iLogger $
      "Answered callback query with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeAnswerCallbackQueryRequest ::
     Token -> ApiVersion -> ChatId -> QueryId -> QueryData -> Request
makeAnswerCallbackQueryRequest token apiVersion chatId queryId queryData =
  let path = "/method/messages.sendMessageEventAnswer"
      host = "api.vk.com"
      queryString =
        [ ("user_id", Just $ BSC.pack $ show chatId)
        , ("event_id", Just $ encodeUtf8 queryId)
        , ( "event_data"
          , Just $ encodeUtf8 $ "Set repeat count to: " <> queryData)
        , ("access_token", Just $ encodeUtf8 token)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
iAnswerHelpCommand :: IHandle -> ChatId -> IO ()
iAnswerHelpCommand IHandle {..} chatId = do
  Logger.info iLogger "Sending answer to /help command..."
  let request =
        makeAnswerHelpCommandRequest
          (cToken iConfig)
          (cApiVersion iConfig)
          chatId
          (cHelpMessage iConfig)
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> Logger.info iLogger "Successfully answered to /help command"
    code ->
      Logger.warning iLogger $
      "Answered to /help command with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeAnswerHelpCommandRequest ::
     Token -> ApiVersion -> ChatId -> HelpMessage -> Request
makeAnswerHelpCommandRequest token apiVersion chatId helpMessage =
  let path = "/method/messages.send"
      host = "api.vk.com"
      queryString =
        [ ("user_id", Just $ BSC.pack $ show chatId)
        , ("message", Just $ encodeUtf8 helpMessage)
        , ("random_id", Just "0")
        , ("access_token", Just $ encodeUtf8 token)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

--------------------------------------------------------------------------------
iAnswerRepeatCommand :: IHandle -> ChatId -> IO ()
iAnswerRepeatCommand IHandle {..} chatId = do
  Logger.info iLogger "Sending answer to /repeat command..."
  let request = undefined
  response <- httpLBS request
  case getResponseStatusCode response of
    200 -> Logger.info iLogger "Successfully answered to /repeat command"
    code ->
      Logger.warning iLogger $
      "Answered to /repeat command with code: " <> pack (show code)

--------------------------------------------------------------------------------
makeAnswerRepeatCommandRequest ::
     Token -> ApiVersion -> ChatId -> RepeatMessage -> Request
makeAnswerRepeatCommandRequest token apiVersion chatId repeatMessage =
  let path = "/method/messages.send"
      host = "api.vk.com"
      queryString =
        [ ("user_id", Just $ BSC.pack $ show chatId)
        , ("message", Just $ encodeUtf8 repeatMessage)
        , ("keyboard", Just $ BSC.pack $ unpackChars $ A.encode keyboard)
        , ("random_id", Just "0")
        , ("access_token", Just $ encodeUtf8 token)
        , ("v", Just $ encodeUtf8 apiVersion)
        ]
   in setRequestPath path $
      setRequestHost host $ setRequestQueryString queryString defaultRequest

keyboard :: Keyboard
keyboard = Keyboard [buttons]

--------------------------------------------------------------------------------
newtype Keyboard =
  Keyboard
    { kButtons :: [[Button]]
    }
  deriving (Show)

instance A.ToJSON Keyboard where
  toJSON Keyboard {..} = A.object ["buttons" A..= kButtons]

--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
data Button =
  Button
    { bAction :: ButtonAction
    , bColor :: Text
    }
  deriving (Show)

instance A.ToJSON Button where
  toJSON Button {..} = A.object ["action" A..= bAction, "color" A..= bColor]

--------------------------------------------------------------------------------
data ButtonAction =
  ButtonAction
    { baType :: Text
    , baLabel :: Text
    , baPayload :: Text
    }
  deriving (Show)

instance A.ToJSON ButtonAction where
  toJSON ButtonAction {..} =
    A.object
      ["type" A..= baType, "label" A..= baLabel, "payload" A..= baPayload]
