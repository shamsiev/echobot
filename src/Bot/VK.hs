module Bot.VK
  ( new
  , parseConfig
  ) where

import           Bot           (Handle)
import           Bot.VK.Config (Config)
import           Data.Aeson    (eitherDecodeFileStrict)
import qualified Logger

new :: Config -> Logger.Handle -> IO Handle
new = undefined

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config
