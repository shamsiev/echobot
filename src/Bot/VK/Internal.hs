{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK.Internal where

import Bot
  ( ChatId
  , Event(EventMedia, EventMessage, EventQuery)
  , Media(MediaAudio, MediaDocument, MediaPhoto, MediaSticker,
      MediaVideo)
  )
import Control.Lens ((&), (.~))
import Data.Aeson (FromJSON(parseJSON), (.:), (.:?), withObject)
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Data.Text (Text, pack)

--------------------------------------------------------------------------------
-- query is not defined yet
updateToEvent :: Update -> Maybe Event
updateToEvent Update {..}
  | isJust $ uMessage uObject =
    let message = fromJust $ uMessage uObject
     in case mAttachments message of
          [] -> Just $ EventMessage (mFromId message) (mText message)
          attachs ->
            Just $
            EventMedia
              (mFromId message)
              (mText message)
              (mapMaybe attachmentToMedia attachs)
  | otherwise = Nothing

attachmentToMedia :: Attachment -> Maybe Media
attachmentToMedia Attachment {..}
  | isJust aPhoto = Just $ MediaPhoto (fileToText "photo" (fromJust aPhoto)) ""
  | isJust aAudio = Just $ MediaAudio (fileToText "audio" (fromJust aAudio)) ""
  | isJust aDocument =
    Just $ MediaDocument (fileToText "doc" (fromJust aDocument)) ""
  | isJust aVideo = Just $ MediaVideo (fileToText "video" (fromJust aVideo)) ""
  | isJust aSticker =
    Just $ MediaSticker (pack $ show $ sId $ fromJust aSticker)
  | otherwise = Nothing

fileToText :: Text -> File -> Text
fileToText name File {..} =
  case fAccessKey of
    Nothing -> name <> pack (show fOwnerId) <> "_" <> pack (show fId)
    Just key ->
      name <> pack (show fOwnerId) <> "_" <> pack (show fId) <> "_" <> key

--------------------------------------------------------------------------------
data LongPollServer =
  LongPollServer
    { lpsKey :: Text
    , lpsServer :: Text
    , lpsTS :: Text
    }
  deriving (Show)

instance FromJSON LongPollServer where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.LongPollServer" $ \o ->
      LongPollServer <$> o .: "key" <*> o .: "server" <*> o .: "ts"

--------------------------------------------------------------------------------
newtype LPSResponse =
  LPSResponse
    { lpsResponse :: LongPollServer
    }
  deriving (Show)

instance FromJSON LPSResponse where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.LPSResponse" $ \o ->
      LPSResponse <$> o .: "response"

--------------------------------------------------------------------------------
newtype Updates =
  Updates
    { uUpdates :: [Update]
    }
  deriving (Show)

instance FromJSON Updates where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Updates" $ \o ->
      Updates <$> o .: "updates"

--------------------------------------------------------------------------------
newtype Update =
  Update
    { uObject :: UObject
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Update" $ \o ->
      Update <$> o .: "object"

--------------------------------------------------------------------------------
newtype UObject =
  UObject
    { uMessage :: Maybe Message
    }
  deriving (Show)

instance FromJSON UObject where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.UObject" $ \o ->
      UObject <$> o .:? "message"

--------------------------------------------------------------------------------
data Message =
  Message
    { mFromId :: Int
    , mText :: Text
    , mAttachments :: [Attachment]
    }
  deriving (Show)

instance FromJSON Message where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Message" $ \o ->
      Message <$> o .: "from_id" <*> o .: "text" <*> o .: "attachments"

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

instance FromJSON Attachment where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Attachment" $ \o ->
      Attachment <$> o .:? "photo" <*> o .:? "audio" <*> o .:? "sticker" <*>
      o .:? "doc" <*>
      o .:? "video"

--------------------------------------------------------------------------------
data File =
  File
    { fAccessKey :: Maybe Text
    , fId :: Int
    , fOwnerId :: Int
    }
  deriving (Show)

instance FromJSON File where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.File" $ \o ->
      File <$> o .:? "access_key" <*> o .: "id" <*> o .: "owner_id"

--------------------------------------------------------------------------------
newtype Sticker =
  Sticker
    { sId :: Int
    }
  deriving (Show)

instance FromJSON Sticker where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Sticker" $ \o ->
      Sticker <$> o .: "sticker_id"
