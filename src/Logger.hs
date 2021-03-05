module Logger
  ( Handle(..)
  , Logger(..)
  , Level(..)
  , Time
  , Message
  , debug
  , info
  , warning
  , error
  ) where

import Data.Text (Text)
import Prelude hiding (error, log)
import Text.Printf (printf)

type Time = String

type Message = Text

data Level
  = Debug
  | Info
  | Warning
  | Error
  deriving (Eq, Ord)

instance Show Level where
  show Debug = "DEBUG"
  show Info = " INFO"
  show Warning = " WARN"
  show Error = "ERROR"

data Logger
  = NoLogger
  | Logger Time Level Message
  deriving (Eq)

instance Show Logger where
  show NoLogger = "NoLogger"
  show (Logger time level message) =
    printf "[%s %s] %s" time (show level) message

newtype Handle r =
  Handle
    { log :: Level -> Message -> IO r
    }

debug :: Handle r -> Message -> IO r
debug = (`log` Debug)

info :: Handle r -> Message -> IO r
info = (`log` Info)

warning :: Handle r -> Message -> IO r
warning = (`log` Warning)

error :: Handle r -> Message -> IO r
error = (`log` Error)
