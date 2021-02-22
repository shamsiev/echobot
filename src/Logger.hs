{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( Severity(..)
  , Handle(..)
  , debug
  , info
  , warning
  , error
  ) where

import Data.Aeson (FromJSON(..), withText)
import Data.Text (unpack)
import Prelude hiding (error, log)

data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show Severity where
  show Debug = "[ DEBUG ]"
  show Info = "[ INFO  ]"
  show Warning = "[ WARN  ]"
  show Error = "[ ERROR ]"

newtype Handle =
  Handle
    { log :: Severity -> String -> IO ()
    }

debug :: Handle -> String -> IO ()
debug = (`log` Debug)

info :: Handle -> String -> IO ()
info = (`log` Info)

warning :: Handle -> String -> IO ()
warning = (`log` Warning)

error :: Handle -> String -> IO ()
error = (`log` Error)

instance FromJSON Severity where
  parseJSON =
    withText "FromJSON Logger.Severity" $ \t ->
      case t of
        "debug" -> pure Debug
        "info" -> pure Info
        "warning" -> pure Warning
        "error" -> pure Error
        _ -> fail $ "Unkown severity: " ++ unpack t
