{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Bot.Telegram where

import           Bot
import           Control.Monad.Reader
import           Data.Text            (Text)
import qualified Logger

data TelegramEnv =
    TelegramEnv
    { tgToken          :: Text
    , tgTimeout        :: Int
    , tgcKeyboardWidth :: Int
    , tgOffset         :: Int
    }

newtype TelegramBot r =
    TelegramBot
    { telegramBot :: ReaderT TelegramEnv IO r
    }
    deriving (Functor,Applicative,Monad,MonadIO,MonadReader TelegramEnv)

instance Bot TelegramBot where
    poll = tgPoll

    sendMessage = tgSendMessage

    sendMedia = tgSendMedia

    possessMedia = tgPossessMedia

    updateMessage = tgUpdateMessage

    answerQuery = tgAnswerQuery

tgPoll :: TelegramBot [Event]
tgPoll = undefined

tgSendMessage
    :: ChatId -> Text -> [QueryButton] -> TelegramBot (Either Text MessageId)
tgSendMessage = undefined

tgSendMedia
    :: ChatId -> Text -> [SendableMedia] -> TelegramBot (Either Text ())
tgSendMedia = undefined

tgPossessMedia :: ChatId -> ForeignMedia -> TelegramBot PossessMediaOutcome
tgPossessMedia = undefined

tgUpdateMessage :: ChatId
                -> MessageId
                -> Text
                -> [QueryButton]
                -> TelegramBot (Either Text ())
tgUpdateMessage = undefined

tgAnswerQuery :: QueryId -> Text -> TelegramBot (Either Text ())
tgAnswerQuery = undefined
