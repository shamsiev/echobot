module Logger.Console
  ( new
  ) where

import Control.Monad (Monad(return), when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Handle(..), Level, Logger(Logger))
import Prelude hiding (log)

new :: Level -> IO (Handle ())
new minLevel =
  return
    Handle
      { log =
          \level message ->
            when (level >= minLevel) $ do
              time <- getCurrentTime
              let timestr = formatTime defaultTimeLocale "%F %T" time
              let logger = Logger timestr level message
              print logger
      }
