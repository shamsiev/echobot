module Bot.VK where

import Bot
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Logger
import qualified Web

--------------------------------------------------------------------------------
type Counter = Int

type Counters = M.Map ChatId Counter

--------------------------------------------------------------------------------
data IHandle =
  IHandle
    { iAccessKey :: Text
    , iGroupId :: Text
    , iApiVersion :: Text
    , iHelpMessage :: Text
    , iRepeatCount :: Text
    , iCounters :: IORef Counters
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
vkGetEvents :: IHandle -> IO [Event]
vkGetEvents = undefined

--------------------------------------------------------------------------------
vkProcessEvents :: IHandle -> [Event] -> IO ()
vkProcessEvents = undefined
