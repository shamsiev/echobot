module Bot.Telegram
  ( new
  , parseConfig
  ) where

import           Bot                 (Handle (Handle))
import           Bot.Telegram.Config (Config)
import           Control.Monad       (forever)
import           Data.Aeson          (eitherDecodeFileStrict)
import           Data.IORef          (IORef, newIORef)
import qualified Logger

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = do
  offset <- newIORef 0
  counters <- newIORef []
  return $ Handle $ forever $ telegram config hLogger offset counters

telegram :: Config -> Logger.Handle -> IORef Int -> IORef [(Int, Int)] -> IO ()
telegram = undefined

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config
