{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram.Internal where

import Bot
  ( Caption
  , Event(EventMedia, EventMessage, EventQuery)
  , FileId
  , Media(MediaAnimation, MediaAudio, MediaDocument, MediaPhoto,
      MediaSticker, MediaVideo, MediaVoice)
  , QueryData
  , QueryId
  )
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), withObject)
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Text (Text)

--------------------------------------------------------------------------------
updateToEvent :: Update -> Maybe Event
updateToEvent Update {..}
  | isJust uQuery = queryToEvent $ fromJust uQuery
  | isJust uMessage = messageToEvent $ fromJust uMessage
  | otherwise = Nothing

--------------------------------------------------------------------------------
messageToEvent :: Message -> Maybe Event
messageToEvent Message {..}
  | isJust mText = Just $ EventMessage (cId mChat) (fromJust mText)
  | isJust mSticker =
    Just $ EventMedia (cId mChat) (MediaSticker (fId $ fromJust mSticker))
  | isJust mAnimation =
    Just $
    EventMedia
      (cId mChat)
      (MediaAnimation (fId $ fromJust mAnimation) (fromMaybe "" mCaption))
  | isJust mDocument =
    Just $
    EventMedia
      (cId mChat)
      (MediaDocument (fId $ fromJust mDocument) (fromMaybe "" mCaption))
  | isJust mPhoto =
    Just $
    EventMedia
      (cId mChat)
      (MediaPhoto (fId $ head $ fromJust mPhoto) (fromMaybe "" mCaption))
  | isJust mVideo =
    Just $
    EventMedia
      (cId mChat)
      (MediaVideo (fId $ fromJust mVideo) (fromMaybe "" mCaption))
  | isJust mAudio =
    Just $
    EventMedia
      (cId mChat)
      (MediaAudio (fId $ fromJust mAudio) (fromMaybe "" mCaption))
  | isJust mVoice =
    Just $
    EventMedia
      (cId mChat)
      (MediaVoice (fId $ fromJust mVoice) (fromMaybe "" mCaption))
  | otherwise = Nothing

--------------------------------------------------------------------------------
queryToEvent :: Query -> Maybe Event
queryToEvent Query {..}
  | isJust qData = Just $ EventQuery (cId qFrom) qId (fromJust qData)
  | otherwise = Nothing

--------------------------------------------------------------------------------
newtype Updates =
  Updates
    { uResult :: [Update]
    }
  deriving (Show)

instance FromJSON Updates where
  parseJSON =
    withObject "Bot.Telegram.Internal.Updates" $ \o -> Updates <$> o .: "result"

--------------------------------------------------------------------------------
data Update =
  Update
    { uUpdateId :: Int
    , uMessage :: Maybe Message
    , uQuery :: Maybe Query
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "Bot.Telegram.Internal.Update" $ \o ->
      Update <$> o .: "update_id" <*> o .:? "message" <*> o .:? "callback_query"

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

instance FromJSON Message where
  parseJSON =
    withObject "Bot.Telegram.Internal.Message" $ \o ->
      Message <$> o .: "chat" <*> o .:? "text" <*> o .:? "caption" <*>
      o .:? "sticker" <*>
      o .:? "animation" <*>
      o .:? "document" <*>
      o .:? "photo" <*>
      o .:? "video" <*>
      o .:? "audio" <*>
      o .:? "voice"

--------------------------------------------------------------------------------
newtype Chat =
  Chat
    { cId :: Int
    }
  deriving (Show)

instance FromJSON Chat where
  parseJSON = withObject "Bot.Telegram.Internal.Chat" $ \o -> Chat <$> o .: "id"

--------------------------------------------------------------------------------
newtype File =
  File
    { fId :: FileId
    }
  deriving (Show)

instance FromJSON File where
  parseJSON =
    withObject "Bot.Telegram.Internal.File" $ \o -> File <$> o .: "file_id"

--------------------------------------------------------------------------------
data Query =
  Query
    { qId :: QueryId
    , qFrom :: Chat
    , qData :: Maybe QueryData
    }
  deriving (Show)

instance FromJSON Query where
  parseJSON =
    withObject "Bot.Telegram.Internal.Query" $ \o ->
      Query <$> o .: "id" <*> o .: "from" <*> o .:? "data"
