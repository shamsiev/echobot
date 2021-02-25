{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}

module Logger.StdLogger where

import           Control.Monad.Reader (MonadIO (..), MonadReader (ask),
                                       ReaderT (..), when)
import           Data.Text            (pack)
import qualified Data.Text.IO         as TextIO
import           Logger               (Logger (..), Severity)

newtype Config =
    Config
    { cSeverity :: Severity
    }

newtype StdLogger r =
    StdLogger
    { stdLogger :: ReaderT Config IO r
    }
    deriving (Functor,Applicative,Monad,MonadIO,MonadReader Config)

instance Logger StdLogger where
    log severity message = do
        Config {..} <- ask
        liftIO
            $ when (severity >= cSeverity)
            $ TextIO.putStrLn
            $ pack (show severity) <> message

runStdLogger :: StdLogger r -> Config -> IO r
runStdLogger (StdLogger logger) = runReaderT logger
