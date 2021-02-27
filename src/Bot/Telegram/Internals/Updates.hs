{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram.Internals.Updates where

import           Bot        (Event (EventMedia, EventMessage, EventQuery),
                             Media (Media),
                             MediaType (MediaAnimation, MediaAudio, MediaDocument, MediaPhoto, MediaSticker, MediaVideo, MediaVoice))
import           Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Text  (Text)

-------------------------------------------------------------------------------
updateToEvent :: Update -> Event
updateToEvent Update {..}
  | isJust uMessage = messageToEvent (fromJust uMessage)
  | isJust uCallbackQuery = callbackQueryToEvent (fromJust uCallbackQuery)

-------------------------------------------------------------------------------
messageToEvent :: Message -> Event
messageToEvent Message {..}
  | isJust mText = EventMessage (uId mFrom) mMessageId (fromJust mText)
  | isJust mAnimation =
      EventMedia
          (uId mFrom)
          [ Media
                MediaAnimation
                (fFileId $ fromJust mAnimation)
                (fromMaybe "" mCaption)]
  | isJust mPhoto =
      EventMedia
          (uId mFrom)
          [ Media
                MediaPhoto
                (fFileId $ head $ fromJust mPhoto)
                (fromMaybe "" mCaption)]
  | isJust mVideo =
      EventMedia
          (uId mFrom)
          [ Media
                MediaVideo
                (fFileId $ fromJust mVideo)
                (fromMaybe "" mCaption)]
  | isJust mAudio =
      EventMedia
          (uId mFrom)
          [ Media
                MediaAudio
                (fFileId $ fromJust mAudio)
                (fromMaybe "" mCaption)]
  | isJust mVoice =
      EventMedia
          (uId mFrom)
          [ Media
                MediaVoice
                (fFileId $ fromJust mVoice)
                (fromMaybe "" mCaption)]
  | isJust mSticker =
      EventMedia
          (uId mFrom)
          [ Media
                MediaSticker
                (fFileId $ fromJust mSticker)
                (fromMaybe "" mCaption)]
  | isJust mDocument =
      EventMedia
          (uId mFrom)
          [ Media
                MediaDocument
                (fFileId $ fromJust mDocument)
                (fromMaybe "" mCaption)]

-------------------------------------------------------------------------------
callbackQueryToEvent :: CallbackQuery -> Event
callbackQueryToEvent CallbackQuery {..} = EventQuery (uId cqFrom) cqId cqData

-------------------------------------------------------------------------------
newtype Updates =
    Updates
    { uResult :: [Update]
    }
    deriving Show

instance FromJSON Updates where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.Updates" $ \o
        -> Updates <$> o .: "result"

-------------------------------------------------------------------------------
data Update =
    Update
    { uUpdate_id     :: Int
    , uMessage       :: Maybe Message
    , uCallbackQuery :: Maybe CallbackQuery
    }
    deriving Show

instance FromJSON Update where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.Update" $ \o
        -> Update <$> o .: "update_id"
        <*> o .:? "message"
        <*> o .:? "callback_query"

-------------------------------------------------------------------------------
data Message =
    Message
    { mMessageId :: Int
    , mFrom      :: User
    , mText      :: Maybe Text
    , mCaption   :: Maybe Text
    , mPhoto     :: Maybe [File]
    , mVideo     :: Maybe File
    , mAudio     :: Maybe File
    , mAnimation :: Maybe File
    , mVoice     :: Maybe File
    , mSticker   :: Maybe File
    , mDocument  :: Maybe File
    }
    deriving Show

instance FromJSON Message where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.Message" $ \o
        -> Message <$> o .: "message_id"
        <*> o .: "from"
        <*> o .:? "text"
        <*> o .:? "caption"
        <*> o .:? "photo"
        <*> o .:? "video"
        <*> o .:? "audio"
        <*> o .:? "animation"
        <*> o .:? "voice"
        <*> o .:? "sticker"
        <*> o .:? "document"

-------------------------------------------------------------------------------
newtype User =
    User
    { uId :: Int
    }
    deriving Show

instance FromJSON User where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.User" $ \o
        -> User <$> o .: "id"

-------------------------------------------------------------------------------
newtype File =
    File
    { fFileId :: Text
    }
    deriving Show

instance FromJSON File where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.File" $ \o
        -> File <$> o .: "file_id"

-------------------------------------------------------------------------------
data CallbackQuery =
    CallbackQuery
    { cqId   :: Text
    , cqFrom :: User
    , cqData :: Text
    }
    deriving Show

instance FromJSON CallbackQuery where
    parseJSON = withObject "FromJSON Bot.Telegram.Internals.CallbackQuery" $ \o
        -> CallbackQuery <$> o .: "id" <*> o .: "from" <*> o .: "data"
