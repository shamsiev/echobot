{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Logger.StdLogger where

import Control.Monad (when)
import Control.Monad.Reader (MonadIO(..), MonadReader(ask), ReaderT(..), when)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Level, Logger(..))
import Text.Printf (printf)

newtype Config =
  Config
    { cLevel :: Level
    }

newtype StdLogger a =
  StdLogger
    { stdLogger :: ReaderT Config IO a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Config)

instance Logger StdLogger where
  log level message = do
    Config {..} <- ask
    when (level >= cLevel) $
      liftIO $ do
        time <- getCurrentTime
        let timestr = formatTime defaultTimeLocale "%F %T" time
        TextIO.putStrLn $
          pack $ printf "[%s %s] %s" (show level) timestr message

runStdLogger :: StdLogger a -> Config -> IO a
runStdLogger logger = runReaderT (stdLogger logger)
