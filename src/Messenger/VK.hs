{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messenger.VK where

import Control.Applicative
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text, pack)
import Data.Text.Encoding
import qualified Logger
import Messenger
import Network.HTTP.Simple

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
    , iOCounters :: IORef Counters
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
       [ eventMessage message
       , eventHelpCommand message
       , eventRepeatCommand message
       , eventMedia message
       , eventQuery message
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
iSendMessage = undefined

--------------------------------------------------------------------------------
iSendMedia :: IHandle -> ChatId -> Media -> IO ()
iSendMedia = undefined

--------------------------------------------------------------------------------
iAnswerQuery :: IHandle -> ChatId -> QueryId -> QueryData -> IO ()
iAnswerQuery = undefined

--------------------------------------------------------------------------------
iAnswerHelpCommand :: IHandle -> ChatId -> IO ()
iAnswerHelpCommand = undefined

--------------------------------------------------------------------------------
iAnswerRepeatCommand :: IHandle -> ChatId -> IO ()
iAnswerRepeatCommand = undefined
