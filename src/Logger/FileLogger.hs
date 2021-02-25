{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Logger.FileLogger where

import           Control.Monad        (when)
import           Control.Monad.Reader (MonadIO (..), MonadReader (ask),
                                       ReaderT (..), when)
import           Data.Text            (pack)
import qualified Data.Text.IO         as TextIO
import           Data.Time.Clock      (getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Logger               (Logger (..), Severity)
import qualified System.IO            as IO

data Config =
    Config
    { cSeverity :: Severity
    , cFilePath :: FilePath
    }

newtype FileLogger r =
    FileLogger
    { fileLogger :: ReaderT Config IO r
    }
    deriving (Functor,Applicative,Monad,MonadIO,MonadReader Config)

instance Logger FileLogger where
    log severity message = do
        Config {..} <- ask
        liftIO $ when (severity >= cSeverity) $ do
            fh <- IO.openFile cFilePath IO.AppendMode
            IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
            time <- getCurrentTime
            let timestr = formatTime defaultTimeLocale "%F %T.%q" time
            TextIO.hPutStrLn fh
                $ pack (show severity ++ timestr ++ ": ") <> message
            IO.hClose fh

runFileLogger :: FileLogger r -> Config -> IO r
runFileLogger (FileLogger logger) = runReaderT logger
