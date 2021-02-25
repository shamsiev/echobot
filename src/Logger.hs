{-# LANGUAGE OverloadedStrings #-}

module Logger where

import           Data.Text (Text, unpack)
import           Data.Yaml (FromJSON (parseJSON), withText)
import           Prelude   hiding (error, log)

newtype Handle =
    Handle
    { log :: Level -> Text -> IO ()
    }

data Level
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq,Ord)

instance Show Level where
    show Debug   = "[ DEBUG ] "
    show Info    = "[ INFO  ] "
    show Warning = "[ WARN  ] "
    show Error   = "[ ERROR ] "

debug :: Handle -> Text -> IO ()
debug = (`log` Debug)

info :: Handle -> Text -> IO ()
info = (`log` Info)

warning :: Handle -> Text -> IO ()
warning = (`log` Warning)

error :: Handle -> Text -> IO ()
error = (`log` Error)

instance FromJSON Level where
    parseJSON = withText "FromJSON Logger.Level" $ \t -> case t of
        "debug"   -> pure Debug
        "info"    -> pure Info
        "warning" -> pure Warning
        "error"   -> pure Error
        _         -> fail $ "Unknown logger level: " ++ unpack t
