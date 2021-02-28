{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Data.Text (Text)
import Data.Yaml (FromJSON(parseJSON), (.:), withObject)

--------------------------------------------------------------------------------
type ChatId = Int

type QueryId = Text

type QueryData = Text

type FileId = Text

type Caption = Text

--------------------------------------------------------------------------------
data Media
  = MediaSticker FileId
  | MediaAnimation FileId Caption
  | MediaDocument FileId Caption
  | MediaPhoto FileId Caption
  | MediaVideo FileId Caption
  | MediaAudio FileId Caption
  | MediaVoice FileId Caption
  deriving (Show, Eq)

--------------------------------------------------------------------------------
data Event
  = EventMessage
      { eChatId :: ChatId
      , eMessage :: Text
      }
  | EventMedia
      { eChatId :: ChatId
      , eMedia :: [Media]
      }
  | EventQuery
      { eChatId :: ChatId
      , eQueryId :: QueryId
      , eUserdata :: QueryData
      }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
data QueryButton =
  QueryButton
    { bTitle :: Text
    , bUserdata :: QueryData
    }
  deriving (Show)

--------------------------------------------------------------------------------
data Config =
  Config
    { cInstance :: Text
    , cHelpMessage :: Text
    , cRepeatMessage :: Text
    , cRepeatCount :: Int
    }
  deriving (Show)

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Bot.Config" $ \o ->
      Config <$> o .: "instance" <*> o .: "help_message" <*>
      o .: "repeat_message" <*>
      o .: "repeat_count"

--------------------------------------------------------------------------------
data Handle =
  Handle
    { getEvents :: IO [Event]
    , processEvents :: [Event] -> IO ()
    }
