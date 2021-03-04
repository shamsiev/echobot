module Logger.StdLogger
  ( new
  ) where

import Control.Monad (Monad(return), when)
import Data.Text (pack, unpack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Handle(Handle), Level)
import Prelude hiding (error)
import Text.Printf (printf)

new :: Level -> IO Handle
new minLevel =
  return $
  Handle $ \level message ->
    when (level >= minLevel) $ do
      time <- getCurrentTime
      let timestr = formatTime defaultTimeLocale "%F %T" time
      TextIO.putStrLn $ pack (printf "[%s %s] %s" (show level) timestr message)
