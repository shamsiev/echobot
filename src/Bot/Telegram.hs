{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram where

import           Bot
import           Control.Lens
import           Data.IORef
import           Data.Text    (Text, pack, unpack)
import qualified Logger
import           Network.Wreq

data Config =
    Config
    { cToken         :: Text
    , cHelpMessage   :: Text
    , cRepeatMessage :: Text
    , cRepeatCount   :: Int
    , cTimeout       :: Int
    }

type Counter = Int

type Counters = [(ChatId, Counter)]

data Telegram =
    Telegram
    { tgConfig   :: Config
    , tgLogger   :: Logger.Handle
    , tgOffset   :: IORef Int
    , tgCounters :: IORef Counters
    }

tgPoll :: Telegram -> IO [Event]
tgPoll Telegram {..} = do
    oldOffset <- readIORef tgOffset
    Logger.debug tgLogger
        $ "Telegram: Current update offset: " <> pack (show oldOffset)
    Logger.info tgLogger "Telegram: Poll..."
    let options =
            defaults
            & param "offset" .~ [pack $ show oldOffset]
            & param "timeout" .~ [pack $ show (cTimeout tgConfig)]
    response <- getWith options
        $ "https://api.telegram.org/bot"
        ++ unpack (cToken tgConfig)
        ++ "/getUpdates"
    undefined

tgSendMessage = undefined

tgSendMedia = undefined

tgAnswerQuery = undefined
