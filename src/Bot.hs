module Bot where

import           Data.Text (Text)

type ChatId = Int

type MessageId = Int

type FileId = Text

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
    deriving (Show,Eq)

data PossessMediaOutcome
    = PossessMediaSuccess !SendableMedia
    | PossessMediaUnknownType !Text
    | PossessMediaUnsupported
    | PossessMediaInternalError
    deriving (Show,Eq)

data Handle =
    Handle
    { poll :: IO [Event]
    , sendMessage
          :: ChatId -> Text -> [QueryButton] -> IO (Either Text MessageId)
    , sendMedia :: ChatId -> Text -> [SendableMedia] -> IO (Either Text ())
    , possessMedia :: ChatId -> ForeignMedia -> IO PossessMediaOutcome
    , updateMessage :: ChatId
                    -> MessageId
                    -> Text
                    -> [QueryButton]
                    -> IO (Either Text ())
    , answerQuery :: QueryId -> Text -> IO (Either Text ())
    }
