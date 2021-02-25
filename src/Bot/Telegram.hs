module Bot.Telegram where

import           Bot
import           Data.IORef
import           Data.Text  (Text)
import qualified Logger

data TelegramBot =
    TelegramBot
    { tgToken         :: Text
    , tgTimeout       :: Int
    , tgKeyboardWidth :: Int
    , tgLogger        :: Logger.Handle
      -- , vkDriver        :: WebDriver.Handle
    , tgOffset        :: IORef Int
    }
