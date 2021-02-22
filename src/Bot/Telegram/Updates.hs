{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Updates where

import           Data.Aeson   (FromJSON (parseJSON), withObject, (.:))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

newtype Updates =
  Updates
    { result :: [Update]
    }
  deriving (Show, Generic, FromJSON)

data Update =
  Update
    { update_id      :: Int
    , callback_query :: CallbackQuery
    , message        :: Message
    }
  deriving (Show, Generic, FromJSON)

data CallbackQuery =
  CallbackQuery
    { cqId              :: Text
    , cqFrom            :: User
    , cqMessage         :: Message
    , cqInlineMessageId :: Text
    , cqChatInstance    :: Text
    , cqData            :: Text
    , cqGameShortName   :: Text
    }
  deriving (Show)

instance FromJSON CallbackQuery where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.CallbackQuery" $ \o ->
      CallbackQuery <$> o .: "id" <*> o .: "from" <*> o .: "message" <*>
      o .: "inline_message_id" <*>
      o .: "chat_instance" <*>
      o .: "data" <*>
      o .: "game_short_name"

data User =
  User
    { uId                      :: Int
    , uIsBot                   :: Bool
    , uFirstName               :: Text
    , uLastName                :: Text
    , uUsername                :: Text
    , uLanguageCode            :: Text
    , uCanJoinGroups           :: Bool
    , uCanReadAllGroupMessages :: Bool
    , uSupportsInlineQueries   :: Bool
    }
  deriving (Show)

instance FromJSON User where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.User" $ \o ->
      User <$> o .: "id" <*> o .: "is_bot" <*> o .: "first_name" <*>
      o .: "last_name" <*>
      o .: "username" <*>
      o .: "language_code" <*>
      o .: "can_join_groups" <*>
      o .: "can_read_all_group_messages" <*>
      o .: "supports_inline_queries"

data Message =
  Message
    { mMessage_id            :: Int
    , mFrom                  :: User
  -- , mSenderChat :: Chat
    , mDate                  :: Int
    , mForwardFrom           :: User
  -- , mForwardFromChat :: Chat
    , mForwardFromMessageId  :: Int
    , mForwardSignature      :: Text
    , mForwardSenderName     :: Text
    , mForwardDate           :: Int
    , mReplyToMessage        :: Message
    , mViaBot                :: User
    , mEditDate              :: Int
    , mMediaGroupId          :: Text
    , mAuthorSignature       :: Text
    , mText                  :: Text
  -- , mEntities :: [MessageEntity]
  -- , mAnimation :: Animation
  -- , mAudio :: Audio
  -- , mDocument :: Docuemnt
  -- , mPhoto :: [PhotoSize]
  -- , mSticker :: Sticker
  -- , mVideo :: Video
  -- , mVideoNote :: VideoNote
  -- , mVoice :: Voice
    , mCaption               :: Text
  -- , mCaptionEntities :: [MessageEntity]
  -- , mContact :: Contact
  -- , mDice :: Dice
  -- , mGame :: Game
  -- , mPoll :: Poll
  -- , mVenue :: Venue
  -- , mLocation :: Location
    , mNewChatMembers        :: [User]
    , mLeftChatMember        :: User
    , mNewChatTitle          :: Text
  -- , mNewChatPhoto :: [PhotoSize]
    , mDeleteChatPhoto       :: Bool
    , mGroupChatCreated      :: Bool
    , mSupergroupChatCreated :: Bool
    , mChannelChatCreated    :: Bool
    , mMigrateToChatId       :: Int
    , mMigrateFromChatId     :: Int
    , mPinnedMessage         :: Message
  -- , mInvoice :: Invoice
  -- , mSuccessfulPayment :: SuccessfulPayment
    , mConnectedWebsite      :: Text
  -- , mPassportData :: PassportData
  -- , mProximityAlertTriggered :: ProximityAlertTriggered
  -- , mReplyMarkUp :: InlineKeyboardMarkup
    }
  deriving (Show)

instance FromJSON Message where
  parseJSON = undefined
