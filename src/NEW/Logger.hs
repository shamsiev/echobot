{-# LANGUAGE OverloadedStrings #-}

module NEW.Logger
  ( Handle (..),
    Severity (..),
  )
where

import           Data.Aeson (FromJSON (..), withText)
import           Data.Text  (unpack)
import           Prelude    hiding (error)

data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show Severity where
  show Debug   = "[ DEBUG ] "
  show Info    = "[ INFO  ] "
  show Warning = "[ WARN  ] "
  show Error   = "[ ERROR ] "

data Handle = Handle
  { debug   :: String -> IO (),
    info    :: String -> IO (),
    warning :: String -> IO (),
    error   :: String -> IO ()
  }

instance FromJSON Severity where
  parseJSON = withText "FromJSON Logger.Severity" $ \t -> case t of
    "debug"   -> pure Debug
    "info"    -> pure Info
    "warning" -> pure Warning
    "error"   -> pure Error
    _         -> fail $ "Unkown severity: " ++ unpack t
