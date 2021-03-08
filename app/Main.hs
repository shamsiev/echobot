{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad (forever, when)
import Data.Maybe (fromJust, isNothing)
import Data.Text (Text, unpack)
import Data.Yaml
  ( FromJSON
  , ParseException
  , decodeFileEither
  , prettyPrintParseException
  )
import GHC.Generics (Generic)
import qualified Logger
import qualified Logger.Console
import qualified Logger.File
import qualified Messenger
import qualified Messenger.Telegram as Telegram
import qualified Messenger.VK as VK

main :: IO ()
main = do
  config <-
    either (fail . prettyPrintParseException) return =<<
    (decodeFileEither "config.yaml" :: IO (Either ParseException Config))
  logger <- chooseLogger config
  runMessenger =<< chooseMessenger config logger

chooseMessenger :: Config -> Logger.Handle () -> IO Messenger.Handle
chooseMessenger Config {..} logger =
  case messenger of
    "telegram" ->
      Telegram.new
        (Telegram.Config help_message repeat_message repeat_count timeout token)
        logger
    "vk" -> do
      when (isNothing api_version) $ fail "Api version is not specified"
      when (isNothing group_id) $ fail "Group id is not specified"
      VK.new
        (VK.Config
           help_message
           repeat_message
           repeat_count
           timeout
           token
           (fromJust group_id)
           (fromJust api_version))
        logger
    _ -> fail $ "Unknown messenger: " <> unpack messenger

chooseLogger :: Config -> IO (Logger.Handle ())
chooseLogger Config {..} =
  case log_to of
    "console" -> Logger.Console.new log_level
    "file" ->
      case log_path of
        Nothing -> fail "Path to log file is not specified"
        Just path -> Logger.File.new log_level path

data Config =
  Config
    { messenger :: Text
    , help_message :: Text
    , repeat_message :: Text
    , repeat_count :: Int
    , timeout :: Int
    , token :: Text
    , api_version :: Maybe Text
    , group_id :: Maybe Text
    , log_to :: Text
    , log_level :: Logger.Level
    , log_path :: Maybe FilePath
    }
  deriving (Show, Generic, FromJSON)

runMessenger :: Messenger.Handle -> IO ()
runMessenger handle =
  forever $ do
    events <- Messenger.getEvents handle
    mapM_ (handleEvent handle) events

handleEvent :: Messenger.Handle -> Messenger.Event -> IO ()
handleEvent handle (Messenger.EventMessage chatId text) =
  Messenger.sendMessage handle chatId text
handleEvent handle (Messenger.EventMedia chatId media) =
  Messenger.sendMedia handle chatId media
handleEvent handle (Messenger.EventQuery chatId queryId queryData) =
  Messenger.answerQuery handle chatId queryId queryData
handleEvent handle (Messenger.EventHelpCommand chatId) =
  Messenger.answerHelpCommand handle chatId
handleEvent handle (Messenger.EventRepeatCommand chatId) =
  Messenger.answerRepeatCommand handle chatId
handleEvent _ _ = return ()
