{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.StdLogger
  ( new
  ) where

import Control.Monad (when)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Config(..), Handle(..), Severity)

--------------------------------------------------------------------------------
new :: Config -> IO Handle
new Config {..} =
  return $
  Handle
    (\severity message ->
       when (severity >= cSeverity) $ do
         time <- getCurrentTime
         let timestr = formatTime defaultTimeLocale "%F %T.%q" time
         TextIO.putStrLn $
           pack ("[" ++ timestr ++ show severity ++ "]") <> message)
