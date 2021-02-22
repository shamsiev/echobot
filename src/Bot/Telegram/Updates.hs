{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Updates where

import           Data.Aeson   (FromJSON (parseJSON), withObject, (.:), (.:?))
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
    , callback_query :: Maybe CallbackQuery
    , message        :: Maybe Message
    }
  deriving (Show, Generic, FromJSON)

data CallbackQuery =
  CallbackQuery
    { cqId              :: Text
    , cqFrom            :: User
    , cqMessage         :: Maybe Message
    , cqInlineMessageId :: Maybe Text
    , cqChatInstance    :: Text
    , cqData            :: Maybe Text
    , cqGameShortName   :: Maybe Text
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
    , uLastName                :: Maybe Text
    , uUsername                :: Maybe Text
    , uLanguageCode            :: Maybe Text
    , uCanJoinGroups           :: Maybe Bool
    , uCanReadAllGroupMessages :: Maybe Bool
    , uSupportsInlineQueries   :: Maybe Bool
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
    , mFrom                  :: Maybe User
  -- , mSenderChat :: Maybe Chat
    , mDate                  :: Int
    -- , mChat :: Maybe Chat
    , mForwardFrom           :: Maybe User
  -- , mForwardFromChat :: Maybe Chat
    , mForwardFromMessageId  :: Maybe Int
    , mForwardSignature      :: Maybe Text
    , mForwardSenderName     :: Maybe Text
    , mForwardDate           :: Maybe Int
    , mReplyToMessage        :: Maybe Message
    , mViaBot                :: Maybe User
    , mEditDate              :: Maybe Int
    , mMediaGroupId          :: Maybe Text
    , mAuthorSignature       :: Maybe Text
    , mText                  :: Maybe Text
  -- , mEntities :: Maybe [MessageEntity]
  -- , mAnimation :: Maybe Animation
  -- , mAudio :: Maybe Audio
  -- , mDocument :: Maybe Docuemnt
  -- , mPhoto :: Maybe [PhotoSize]
  -- , mSticker :: Maybe Sticker
  -- , mVideo :: Maybe Video
  -- , mVideoNote :: Maybe VideoNote
  -- , mVoice :: Maybe Voice
    , mCaption               :: Maybe Text
  -- , mCaptionEntities :: Maybe [MessageEntity]
  -- , mContact :: Maybe Contact
  -- , mDice :: Maybe Dice
  -- , mGame :: Maybe Game
  -- , mPoll :: Maybe Poll
  -- , mVenue :: Maybe Venue
  -- , mLocation :: Maybe Location
    , mNewChatMembers        :: Maybe [User]
    , mLeftChatMember        :: Maybe User
    , mNewChatTitle          :: Maybe Text
  -- , mNewChatPhoto :: Maybe [PhotoSize]
    , mDeleteChatPhoto       :: Maybe Bool
    , mGroupChatCreated      :: Maybe Bool
    , mSupergroupChatCreated :: Maybe Bool
    , mChannelChatCreated    :: Maybe Bool
    , mMigrateToChatId       :: Maybe Int
    , mMigrateFromChatId     :: Maybe Int
    , mPinnedMessage         :: Maybe Message
  -- , mInvoice :: Maybe Invoice
  -- , mSuccessfulPayment :: Maybe SuccessfulPayment
    , mConnectedWebsite      :: Maybe Text
  -- , mPassportData :: Maybe PassportData
  -- , mProximityAlertTriggered :: Maybe ProximityAlertTriggered
  -- , mReplyMarkUp :: Maybe InlineKeyboardMarkup
    }
  deriving (Show)

instance FromJSON Message where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.Message" $ \o ->
      Message <$> o .: "message_id" <*> o .:? "from" <*> o .: "date" <*>
      o .:? "forward_from" <*>
      o .:? "forward_from_message_id" <*>
      o .:? "forward_signature" <*>
      o .:? "forward_sender_name" <*>
      o .:? "forward_date" <*>
      o .:? "reply_to_message" <*>
      o .:? "via_bot" <*>
      o .:? "edit_date" <*>
      o .:? "media_group_id" <*>
      o .:? "author_signature" <*>
      o .:? "text" <*>
      o .:? "caption" <*>
      o .:? "new_chat_members" <*>
      o .:? "left_chat_member" <*>
      o .:? "new_chat_title" <*>
      o .:? "delete_chat_photo" <*>
      o .:? "group_chat_created" <*>
      o .:? "supergroup_chat_created" <*>
      o .:? "channel_chat_created" <*>
      o .:? "migrate_to_chat_id" <*>
      o .:? "migrate_from_chat_id" <*>
      o .:? "pinned_message" <*>
      o .:? "connected_website"
