{-# LANGUAGE RecordWildCards #-}

module Logger.FileLogger (new) where

import           Control.Monad    (when)
import           Data.Text        (pack)
import qualified Data.Text.IO     as TextIO
import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Logger           (Handle (..), Severity)
import qualified System.IO        as IO

-------------------------------------------------------------------------------
data Config =
    Config
    { cSeverity :: Severity
    , cFilePath :: FilePath
    }

-------------------------------------------------------------------------------
new :: Config -> IO Handle
new Config {..} =
    return
        Handle
        { log = \severity message -> when (severity >= cSeverity) $ do
              fh <- IO.openFile cFilePath IO.AppendMode
              IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
              time <- getCurrentTime
              let timestr = formatTime defaultTimeLocale "%F %T.%q" time
              TextIO.hPutStrLn fh
                  $ pack (show severity ++ show timestr ++ ": ") <> message
              IO.hClose fh
        }
