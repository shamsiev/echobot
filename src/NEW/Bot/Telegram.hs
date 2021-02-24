{-# LANGUAGE OverloadedStrings #-}

module NEW.Bot.Telegram where

import           Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict,
                             withObject, (.:))
import           Data.Text  (Text)
import           NEW.Bot    (Handle)
import qualified NEW.Logger as Logger

newHandle :: Logger.Handle -> IO Handle
newHandle = undefined

data Config =
    Config
    { cHelpMessage   :: Text
    , cRepeatMessage :: Text
    , cRepeatCount   :: Int
    , cToken         :: Text
    }

parseConfig :: FilePath -> IO Config
parseConfig filePath = eitherDecodeFileStrict filePath >>= either fail return

instance FromJSON Config where
    parseJSON = withObject "FromJSON Bot.Telegram.Config" $ \o -> Config
        <$> o .: "help_message"
        <*> o .: "repeat_message"
        <*> o .: "repeat_count"
        <*> o .: "token"
