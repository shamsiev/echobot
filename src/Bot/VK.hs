{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Bot.VK where

import           Bot
import           Control.Monad.Reader
import           Data.IORef
import           Data.Text            (Text)
import qualified Logger
import qualified System.Random        as Random

data VKEnv =
    VKEnv
    { vkToken         :: Text
    , vkGroupId       :: Int
    , vkTimeout       :: Int
    , vkKeyboardWidth :: Int
    , vkRandomState   :: IORef Random.StdGen
    }

newtype VKBot r =
    VKBot
    { vkBot :: ReaderT VKEnv IO r
    }
    deriving (Functor,Applicative,Monad,MonadIO,MonadReader VKEnv)

instance Bot VKBot where
    poll = vkPoll

    sendMessage = vkSendMessage

    sendMedia = vkSendMedia

    possessMedia = vkPossessMedia

    updateMessage = vkUpdateMessage

    answerQuery = vkAnswerQuery

vkPoll :: VKBot [Event]
vkPoll = undefined

vkSendMessage
    :: ChatId -> Text -> [QueryButton] -> VKBot (Either Text MessageId)
vkSendMessage = undefined

vkSendMedia :: ChatId -> Text -> [SendableMedia] -> VKBot (Either Text ())
vkSendMedia = undefined

vkPossessMedia :: ChatId -> ForeignMedia -> VKBot PossessMediaOutcome
vkPossessMedia = undefined

vkUpdateMessage
    :: ChatId -> MessageId -> Text -> [QueryButton] -> VKBot (Either Text ())
vkUpdateMessage = undefined

vkAnswerQuery :: QueryId -> Text -> VKBot (Either Text ())
vkAnswerQuery = undefined
