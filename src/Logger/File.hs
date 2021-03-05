module Logger.File where

import Control.Monad (when)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Handle(Handle), Level, Logger(Logger), Message)
import qualified System.IO as IO

new :: Level -> FilePath -> IO (Handle ())
new minLevel filePath = return (Handle (file minLevel filePath))

file :: Level -> FilePath -> Level -> Message -> IO ()
file minLevel filePath level message =
  when (level >= minLevel) $ do
    fh <- IO.openFile filePath IO.AppendMode
    IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
    time <- getCurrentTime
    let timestr = formatTime defaultTimeLocale "%F %T" time
    let logger = Logger timestr level message
    IO.print logger
    IO.hClose fh
