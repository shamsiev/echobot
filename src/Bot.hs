module Bot where

import           Data.Text (Text)

-------------------------------------------------------------------------------
type ChatId = Int

type QueryUserdata = Text

type MessageId = Int

type QueryId = Text

type QueryData = Text

type FileId = Text

type Caption = Text

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
data Media = Media !MediaType !FileId !Caption
    deriving (Show,Eq)

-------------------------------------------------------------------------------
data Event
    = EventMessage
      { eChatId    :: !ChatId
      , eMessageId :: !MessageId
      , eMessage   :: !Text
      }
    | EventMedia
      { eChatId :: !ChatId
      , eMedia  :: [Media]
      }
    | EventQuery
      { eChatId   :: !ChatId
      , eQueryId  :: !QueryId
      , eUserdata :: !QueryUserdata
      }
    deriving (Show,Eq)

-------------------------------------------------------------------------------
data QueryButton =
    QueryButton
    { bTitle    :: !Text
    , bUserdata :: !QueryUserdata
    }

-------------------------------------------------------------------------------
data Handle =
    Handle
    { poll        :: IO [Event]
    , sendMessage :: ChatId -> Text -> [QueryButton] -> IO ()
    , sendMedia   :: ChatId -> [Media] -> IO ()
    , answerQuery :: QueryId -> QueryData -> IO ()
    }
