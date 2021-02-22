module Bot.Telegram
  ( new
  , parseConfig
  ) where

import           Bot                 (Handle)
import           Bot.Telegram.Config (Config)
import           Data.Aeson          (eitherDecodeFileStrict)
import qualified Logger

new :: Config -> Logger.Handle -> IO Handle
new = undefined

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config
