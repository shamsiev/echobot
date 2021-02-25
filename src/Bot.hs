module Bot where

import           Data.Text (Text)

type ChatId = Int

type MessageId = Int

type QueryId = Text

type QueryUserdata = Text

data MediaType
    = MediaPhoto
    | MediaVideo
    | MediaAudio
    | MediaAnimation
    | MediaVoice
    | MediaSticker
    | MediaDocument
    | MediaUnknown
    deriving (Show,Eq)

data ForeignMedia = ForeignMedia !MediaType !Text !Text
    deriving (Show,Eq)

data SendableMedia = SendableMedia !MediaType !Text
    deriving (Show,Eq)

data Event
    = EventMessage
      { eChatId    :: !ChatId
      , eMessageId :: !MessageId
      , eMessage   :: !Text
      }
    | EventMedia
      { eChatId  :: !ChatId
      , eCaption :: !Text
      , eMedia   :: [ForeignMedia]
      }
    | EventQuery
      { eChatId    :: !ChatId
      , eMessageId :: !MessageId
      , eQueryId   :: !QueryId
      , eUserdata  :: !QueryUserdata
      }
    deriving (Show,Eq)

data QueryButton =
    QueryButton
    { bTitle    :: !Text
    , bUserdata :: !QueryUserdata
    }

data PossessMediaOutcome
    = PossessMediaSuccess !SendableMedia
    | PossessMediaUnknownType !Text
    | PossessMediaUnsupported
    | PossessMediaInternalError
    deriving (Show,Eq)

class Monad m => Bot m where
    poll :: m [Event]
    sendMessage :: ChatId -> Text -> [QueryButton] -> m (Either Text MessageId)
    sendMedia :: ChatId -> Text -> [SendableMedia] -> m (Either Text ())
    possessMedia :: ChatId -> ForeignMedia -> m PossessMediaOutcome
    updateMessage
        :: ChatId -> MessageId -> Text -> [QueryButton] -> m (Either Text ())
    answerQuery :: QueryId -> Text -> m (Either Text ())
