{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bot
import qualified Bot.Telegram as Telegram
import qualified Bot.VK as VK
import Control.Monad (forever, when)
import Data.Maybe (fromJust, isNothing)
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
import System.Environment (getArgs)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    (configPath:_) -> do
      config <-
        either (fail . prettyPrintParseException) return =<<
        (decodeFileEither configPath :: IO (Either ParseException Config))
      logger <- readLogger config
      let botInstance = Bot.cInstance (cBot config)
      case botInstance of
        "telegram" -> do
          when (isNothing $ cTelegram config) $
            fail "No 'telegram' in config file"
          handle <-
            Telegram.new logger (cBot config) (fromJust $ cTelegram config)
          forever $ Bot.getEvents handle >>= Bot.processEvents handle
        "vk" -> do
          when (isNothing $ cVK config) $ fail "No 'vk' in config file"
          handle <- VK.new logger (cBot config) (fromJust $ cVK config)
          forever $ Bot.getEvents handle >>= Bot.processEvents handle
        _ -> fail $ "Unknown bot instance: " ++ unpack botInstance

readLogger :: Config -> IO Logger.Handle
readLogger config = do
  let loggerType = Logger.cType (cLogger config)
  case loggerType of
    "console" -> StdLogger.new (cLogger config)
    "file" -> FileLogger.new (cLogger config)
    _ -> fail $ "Unknown logger type: " ++ unpack loggerType

--------------------------------------------------------------------------------
data Config =
  Config
    { cLogger :: Logger.Config
    , cBot :: Bot.Config
    , cTelegram :: Maybe Telegram.IConfig
    , cVK :: Maybe VK.IConfig
    }
  deriving (Show)

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Main.Config" $ \o ->
      Config <$> o .: "logger" <*> o .: "bot" <*> o .:? "telegram" <*>
      o .:? "vk"
