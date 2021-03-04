module Logger
  ( Handle(..)
  , Level(..)
  , debug
  , info
  , warning
  , error
  ) where

import Data.Text (Text)
import qualified Data.Text.IO as TextIO
import Prelude hiding (error, log)

newtype Handle =
  Handle
    { log :: Level -> Text -> IO ()
    }

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show Level where
  show Debug = "DEBUG"
  show Info = "INFO "
  show Warning = "WARN "
  show Error = "ERROR"

debug :: Handle -> Text -> IO ()
debug = (`log` Debug)

info :: Handle -> Text -> IO ()
info = (`log` Info)

warning :: Handle -> Text -> IO ()
warning = (`log` Warning)

error :: Handle -> Text -> IO ()
error = (`log` Error)
