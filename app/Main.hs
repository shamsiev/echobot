{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot
import qualified Bot.Telegram as Telegram
import Control.Monad (forever)
import Data.Text (Text, unpack)
import Data.Yaml
  ( FromJSON(parseJSON)
  , ParseException
  , (.:)
  , (.:?)
  , decodeFileEither
  , prettyPrintParseException
  , withObject
  )
import qualified Logger
import qualified Logger.FileLogger as FileLogger
import qualified Logger.StdLogger as StdLogger

--------------------------------------------------------------------------------
main :: IO ()
main = do
  Config {..} <-
    either (fail . prettyPrintParseException) return =<<
    (decodeFileEither "config.yaml" :: IO (Either ParseException Config))
  case Logger.cType cLogger of
    "console" -> do
      logger <- StdLogger.new cLogger
      case cTelegram of
        Nothing -> fail "AJOIASJFIO??)))"
        Just tg -> do
          handle <- Telegram.new logger cBot tg
          forever $ Bot.getEvents handle >>= (handle `Bot.processEvents`)
    "file" -> undefined
    loggerType -> fail $ "Unknown logger type: " ++ unpack loggerType

--------------------------------------------------------------------------------
data Config =
  Config
    { cLogger :: Logger.Config
    , cBot :: Bot.Config
    , cTelegram :: Maybe Telegram.IConfig
    }
  deriving (Show)

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Main.Config" $ \o ->
      Config <$> o .: "logger" <*> o .: "bot" <*> o .:? "telegram"
