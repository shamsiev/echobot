{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( Handle(..)
  , Severity(..)
  , Config(..)
  , debug
  , info
  , warning
  , error
  ) where

import Data.Text (Text)
import Data.Yaml (FromJSON(parseJSON), (.:), withObject, withText)
import Prelude hiding (error, log)

--------------------------------------------------------------------------------
newtype Handle =
  Handle
    { log :: Severity -> Text -> IO ()
    }

--------------------------------------------------------------------------------
data Config =
  Config
    { cType :: Text
    , cSeverity :: Severity
    , cFilePath :: Maybe FilePath
    }
  deriving (Show)

--------------------------------------------------------------------------------
instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Logger.Config" $ \o ->
      Config <$> o .: "type" <*> o .: "level" <*> o .: "file_path"

--------------------------------------------------------------------------------
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Ord, Eq)

instance Show Severity where
  show Debug = " [ DEBUG ] "
  show Info = " [ INFO  ] "
  show Warning = " [ WARN  ] "
  show Error = " [ ERROR ] "

instance FromJSON Severity where
  parseJSON =
    withText "FromJSON Logger.Severity" $ \t ->
      case t of
        "debug" -> pure Debug
        "info" -> pure Info
        "warning" -> pure Warning
        "error" -> pure Error
        _ -> fail $ "Unknown logger level: " ++ show t

--------------------------------------------------------------------------------
debug :: Handle -> Text -> IO ()
debug = (`log` Debug)

info :: Handle -> Text -> IO ()
info = (`log` Info)

warning :: Handle -> Text -> IO ()
warning = (`log` Warning)

error :: Handle -> Text -> IO ()
error = (`log` Error)
