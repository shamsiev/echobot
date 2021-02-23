module Bot.Telegram.HandleMessage
  ( Handle(..)
  , Token
  ) where

import           Bot.Telegram.Updates (Message)
import           Data.Text            (Text)

type Token = Text

newtype Handle =
  Handle
    { handle :: Token -> Message -> IO ()
    }
