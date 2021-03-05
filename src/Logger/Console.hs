module Logger.Console where

import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Handle(Handle), Level, Logger(Logger), Message)

new :: Level -> IO (Handle ())
new minLevel = return (Handle (console minLevel))

console :: Level -> Level -> Message -> IO ()
console minLevel level message =
  when (level >= minLevel) $ do
    time <- getCurrentTime
    let timestr = formatTime defaultTimeLocale "%F %T" time
    let logger = Logger timestr level message
    print logger
