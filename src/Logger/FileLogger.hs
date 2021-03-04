{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger.FileLogger
  ( new
  ) where

import Control.Monad (when)
import Data.Text (pack)
import qualified Data.Text.IO as TextIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Logger (Config(..), Handle(Handle))
import qualified System.IO as IO

--------------------------------------------------------------------------------
new :: Config -> IO Handle
new Config {..} =
  case cFilePath of
    Nothing -> fail "fila_path is not specified"
    Just filePath ->
      return $
      Handle
        (\severity message ->
           when (severity >= cSeverity) $ do
             fh <- IO.openFile filePath IO.AppendMode
             IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
             time <- getCurrentTime
             let timestr = formatTime defaultTimeLocale "%F %T.%q" time
             TextIO.hPutStrLn fh $
               pack ("[" ++ timestr ++ show severity ++ "]") <> message
             IO.hClose fh)
