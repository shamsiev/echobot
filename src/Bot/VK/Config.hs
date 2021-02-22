{-# LANGUAGE OverloadedStrings #-}

module Bot.VK.Config
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
    , cApiVersion    :: Text
    , cGroupId       :: Text
    }

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Bot.VK.Config" $ \o ->
      Config <$> o .: "help_message" <*> o .: "repeat_message" <*>
      o .: "repeat_count" <*>
      o .: "token" <*>
      o .: "api_version" <*>
      o .: "group_id"
