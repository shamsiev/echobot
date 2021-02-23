module Bot.Telegram.HandleMessage
  ( Handle(..)
  , ChatId
  , Counter
  , Counters
  ) where

import           Bot.Telegram.Updates (Message)
import qualified Data.Map.Strict      as M

type ChatId = Int

type Counter = Int

type Counters = M.Map ChatId Counter

newtype Handle =
  Handle
    { handle :: Message -> IO ()
    }
