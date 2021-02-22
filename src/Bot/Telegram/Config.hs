{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Config
  ( Config(..)
  ) where

import           Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import           Data.Text  (Text)

data Config =
  Config
    { cHelpMessage   :: Text
    , cRepeatMessage :: Text
    , cRepeatCount   :: Int
    , cToken         :: Text
    }

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Config" $ \o ->
      Config <$> o .: "help_message" <*> o .: "repeat_message" <*>
      o .: "repeat_count" <*>
      o .: "token"
