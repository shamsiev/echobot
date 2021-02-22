module Bot.Telegram.HandleMessage where

import           Bot.Telegram.Updates (Message)

newtype Handle =
  Handle
    { handle :: Message -> IO ()
    }
