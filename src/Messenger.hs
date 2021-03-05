module Messenger where

import Data.Text (Text)

type ChatId = Int

type Caption = Text

type FileId = Text

type QueryId = Text

type QueryData = Text

data MediaType
  = StickerMedia
  | VoiceMedia
  | AnimationMedia
  | DocumentMedia
  | PhotoMedia
  | VideoMedia
  | AudioMedia
  deriving (Eq, Show)

data TelegramEvent
  = TelegramMessage ChatId Text
  | TelegramMediaCaption MediaType ChatId Caption FileId
  | TelegramMedia MediaType ChatId FileId
  | TelegramQuery ChatId QueryId QueryData
  deriving (Eq, Show)

type OwnerId = Int

type MediaId = Int

data VKMediaFile =
  VKMediaFile MediaType OwnerId MediaId
  deriving (Eq, Show)

data VKEvent
  = VKMessage ChatId Text
  | VKMedia ChatId [VKMediaFile]
  | VKMessageWithMedia ChatId Text [VKMediaFile]
  | VKQuery ChatId QueryId QueryId
  | HelpCommand ChatId
  | RepeatCommnad ChatId
  deriving (Eq, Show)

data Event
  = TelegramEvent TelegramEvent
  | VKEvent VKEvent
  deriving (Eq, Show)

data HandledEvent
  = HTelegramMessage
  | HTelegramMedia
  | HTelegramQuery
  | HVKMessage
  | HVKMedia
  | HVKMessageWithMedia
  | HHelpCommand
  | HRepeatCommand
  deriving (Eq, Show)

data Handle =
  Handle
    { getEvent :: IO [Event]
    , handleEvent :: Event -> IO HandledEvent
    }
