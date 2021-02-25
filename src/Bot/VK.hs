module Bot.VK where

import           Bot
import           Data.IORef
import           Data.Text     (Text)
import qualified Logger
import qualified System.Random as Random

data VKBot =
    VKBot
    { vkToken         :: Text
    , vkGroupId       :: Int
    , vkTimeout       :: Int
    , vkKeyboardWidth :: Int
    , vkLogger        :: Logger.Handle
      -- , vkDriver        :: WebDriver.Handle
    , vkPollServer    :: IORef (Maybe VKPollServer)
    , vkRandomState   :: IORef Random.StdGen
    }

data VKPollServer = VKPollServer !Text !Text !Text
