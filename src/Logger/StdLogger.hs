{-# LANGUAGE RecordWildCards #-}

module Logger.StdLogger (new) where

import           Control.Monad (when)
import           Data.Text     (pack)
import qualified Data.Text.IO  as TextIO
import           Logger

newtype Config =
    Config
    { cSeverity :: Severity
    }

new :: Config -> IO Handle
new Config {..} =
    return
        Handle
        { log = \severity message -> when (severity >= cSeverity)
              $ TextIO.putStrLn
              $ pack (show severity) <> message
        }
