{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Logger.File where

import Control.Monad (when)
import Control.Monad.Reader
  ( Functor
  , Monad
  , MonadIO(..)
  , MonadReader(ask)
  , ReaderT(..)
  , (=<<)
  , when
  )
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Level, Logger(..))
import qualified System.IO as IO
import Text.Printf (printf)

data Config =
  Config
    { cLevel :: Level
    , cFilePath :: FilePath
    }

newtype FileLogger a =
  FileLogger
    { fileLogger :: ReaderT Config IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance Logger FileLogger where
  log level message = do
    Config {..} <- ask
    when (level >= cLevel) $
      liftIO $ do
        fh <- IO.openFile cFilePath IO.AppendMode
        IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
        time <- getCurrentTime
        let timestr = formatTime defaultTimeLocale "%F %T" time
        TextIO.hPutStrLn fh $
          pack $ printf "[%s %s] %s" (show level) timestr message
        IO.hClose fh

runFileLogger :: FileLogger a -> Config -> IO a
runFileLogger logger = runReaderT (fileLogger logger)
