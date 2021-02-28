{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.VK.Internal where

import Bot
import Control.Lens ((&), (.~))
import Data.Aeson
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Text (Text, pack)
import Network.Wreq

--------------------------------------------------------------------------------
updateToEvent :: Update -> Maybe Event
updateToEvent Update {..}
  | isJust uPayload = Just $ EventQuery uUserId "" (fromJust uPayload)
  | isJust uAttachments =
    attachmentToEvent uUserId uBody (fromJust uAttachments)
  | otherwise = Just $ EventMessage uUserId uBody

--------------------------------------------------------------------------------
attachmentToEvent :: ChatId -> Text -> [Attachment] -> Maybe Event
attachmentToEvent _ _ [] = Nothing
attachmentToEvent chatId message attachs =
  Just $ EventMedia chatId message (mapMaybe attachmentToMedia attachs)

attachmentToMedia :: Attachment -> Maybe Media
attachmentToMedia Attachment {..}
  | isJust aPhoto =
    case fAccessKey (fromJust aPhoto) of
      Nothing ->
        Just $
        MediaPhoto
          ("photo" <> pack (show $ fOwnerId $ fromJust aPhoto) <> "_" <>
           pack (show $ fId $ fromJust aPhoto))
          ""
      Just key ->
        Just $
        MediaPhoto
          ("photo" <> pack (show $ fOwnerId $ fromJust aPhoto) <> "_" <>
           pack (show $ fId $ fromJust aPhoto) <>
           "_" <>
           key)
          ""
  | isJust aAudio =
    case fAccessKey (fromJust aAudio) of
      Nothing ->
        Just $
        MediaAudio
          ("audio" <> pack (show $ fOwnerId $ fromJust aAudio) <> "_" <>
           pack (show $ fId $ fromJust aAudio))
          ""
      Just key ->
        Just $
        MediaAudio
          ("audio" <> pack (show $ fOwnerId $ fromJust aAudio) <> "_" <>
           pack (show $ fId $ fromJust aAudio) <>
           "_" <>
           key)
          ""
  | isJust aDocument =
    case fAccessKey (fromJust aDocument) of
      Nothing ->
        Just $
        MediaDocument
          ("doc" <> pack (show $ fOwnerId $ fromJust aDocument) <> "_" <>
           pack (show $ fId $ fromJust aDocument))
          ""
      Just key ->
        Just $
        MediaDocument
          ("doc" <> pack (show $ fOwnerId $ fromJust aDocument) <> "_" <>
           pack (show $ fId $ fromJust aDocument) <>
           "_" <>
           key)
          ""
  | isJust aVideo =
    case fAccessKey (fromJust aVideo) of
      Nothing ->
        Just $
        MediaVideo
          ("video" <> pack (show $ fOwnerId $ fromJust aVideo) <> "_" <>
           pack (show $ fId $ fromJust aVideo))
          ""
      Just key ->
        Just $
        MediaVideo
          ("video" <> pack (show $ fOwnerId $ fromJust aVideo) <> "_" <>
           pack (show $ fId $ fromJust aVideo) <>
           "_" <>
           key)
          ""
  | isJust aSticker =
    Just $ MediaSticker $ pack $ show $ sId $ fromJust aSticker
  | otherwise = Nothing

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
    withObject "FromJSON Bot.VK.Internal.Sticker" $ \o -> Sticker <$> o .: "id"
