module Messenger where

import Data.Text (Text)

data Handle =
  Handle
    { getEvents :: IO [Event]
    , sendMessage :: ChatId -> Text -> IO ()
    , sendMedia :: ChatId -> Media -> IO ()
    , answerQuery :: ChatId -> QueryId -> QueryData -> IO ()
    , answerHelpCommand :: ChatId -> IO ()
    , answerRepeatCommand :: ChatId -> IO ()
    }

data Event
  = EventMessage ChatId Text
  | EventMedia ChatId Media
  | EventQuery ChatId QueryId QueryData
  | EventHelpCommand ChatId
  | EventRepeatCommand ChatId
  | UnknownEvent
  deriving (Eq, Show)

data Media
  = TelegramMedia (Maybe Caption) MediaType FileId
  | VKMedia (Maybe Text) [VKMediaFile]
  deriving (Eq, Show)

data MediaType
  = MediaSticker
  | MediaVoice
  | MediaAnimation
  | MediaPhoto
  | MediaVideo
  | MediaAudio
  | MediaDocument
  deriving (Eq, Show)

data VKMediaFile =
  VKMediaFile MediaType OwnerId MediaId
  deriving (Eq, Show)

type OwnerId = Int

type MediaId = Int

type ChatId = Int

type FileId = Text

type Caption = Text

type QueryId = Text

type QueryData = Text

type FailReason = Text
