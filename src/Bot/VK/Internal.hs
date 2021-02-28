{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK.Internal where

import Bot
import Data.Aeson
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)

--------------------------------------------------------------------------------
updateToEvent :: Update -> Maybe Event
updateToEvent Update {..}
  | isJust uPayload = Just $ EventQuery uUserId "" (fromJust uPayload)
  | isJust uAttachments =
    Just $ attachmentToEvent uUserId uBody (fromJust uAttachments)
  | otherwise = Just $ EventMessage uUserId uBody

--------------------------------------------------------------------------------
attachmentToEvent :: ChatId -> Text -> [Attachment] -> Event
attachmentToEvent = undefined

--------------------------------------------------------------------------------
data LongPollServer =
  LongPollServer
    { lpsKey :: Text
    , lpsServer :: Text
    , lpsTS :: Int
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
data Update =
  Update
    { uBody :: Text
    , uAttachments :: Maybe [Attachment]
    , uUserId :: Int
    , uType :: Text
    , uPayload :: Maybe Text
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Update" $ \o ->
      Update <$> o .: "body" <*> o .:? "attachments" <*> o .: "user_id" <*>
      o .: "type" <*>
      o .:? "payload"

--------------------------------------------------------------------------------
data Attachment =
  Attachment
    { aPhoto :: Maybe File
    , aAudio :: Maybe File
    , aSticker :: Maybe Sticker
    , aVoice :: Maybe File
    , aAnimation :: Maybe File
    , aDocument :: Maybe File
    , aVideo :: Maybe File
    }
  deriving (Show)

instance FromJSON Attachment where
  parseJSON =
    withObject "FromJSON Bot.VK.Internal.Attachment" $ \o ->
      Attachment <$> o .:? "photo" <*> o .:? "audio" <*> o .:? "sticker" <*>
      o .:? "doc" <*>
      o .:? "doc" <*>
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
    withObject "FromJSON Bot.VK.Internal.Sticker" $ \o -> Sticker <$> o .: "id"
