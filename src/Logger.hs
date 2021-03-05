module Logger where

import Data.Text (Text)
import Prelude hiding (error, log)

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

class Monad m =>
      Logger m
  where
  log :: Level -> Message -> m ()

debug :: Logger m => Message -> m ()
debug = log Debug

info :: Logger m => Message -> m ()
info = log Info

warning :: Logger m => Message -> m ()
warning = log Warning

error :: Logger m => Message -> m ()
error = log Error
