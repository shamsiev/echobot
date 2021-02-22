module Bot.Telegram.HandleMessage
  ( Handle(..)
  ) where

import           Bot.Telegram.Updates (Message)

newtype Handle =
  Handle
    { handle :: Message -> IO ()
    }
