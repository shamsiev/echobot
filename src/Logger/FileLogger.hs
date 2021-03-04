module Logger.FileLogger
  ( new
  ) where

import Control.Monad (when)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Handle(Handle), Level)
import qualified System.IO as IO
import Text.Printf (printf)

new :: Level -> FilePath -> IO Handle
new minLevel filePath =
  return $
  Handle $ \level message ->
    when (level >= minLevel) $ do
      fh <- IO.openFile filePath IO.AppendMode
      IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
      time <- getCurrentTime
      let timestr = formatTime defaultTimeLocale "%F %T" time
      TextIO.hPutStrLn fh $
        pack $ printf "[%s %s] %s" (show level) timestr message
      IO.hClose fh
