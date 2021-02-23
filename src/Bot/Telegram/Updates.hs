{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram.Updates where

import           Data.Aeson
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
      CallbackQuery <$> o .: "id" <*> o .: "from" <*> o .:? "message" <*>
      o .:? "inline_message_id" <*>
      o .: "chat_instance" <*>
      o .:? "data" <*>
      o .:? "game_short_name"

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
      o .:? "last_name" <*>
      o .:? "username" <*>
      o .:? "language_code" <*>
      o .:? "can_join_groups" <*>
      o .:? "can_read_all_group_messages" <*>
      o .:? "supports_inline_queries"

instance ToJSON User where
  toJSON o =
    object
      [ "id" .= uId o
      , "is_bot" .= uIsBot o
      , "first_name" .= uFirstName o
      , "last_name" .= uLastName o
      , "username" .= uUsername o
      , "language_code" .= uLanguageCode o
      , "can_join_groups" .= uCanJoinGroups o
      , "can_read_all_group_messages" .= uCanReadAllGroupMessages o
      , "supports_inline_queries" .= uSupportsInlineQueries o
      ]

data Message =
  Message
    { mMessage_id           :: Int
    , mFrom                 :: Maybe User
  -- , mSenderChat :: Maybe Chat
    , mDate                 :: Int
  -- , mChat :: Maybe Chat
    , mForwardFrom          :: Maybe User
  -- , mForwardFromChat :: Maybe Chat
    , mForwardFromMessageId :: Maybe Int
    , mForwardSignature     :: Maybe Text
    , mForwardSenderName    :: Maybe Text
    , mForwardDate          :: Maybe Int
    , mReplyToMessage       :: Maybe Message
    , mViaBot               :: Maybe User
    , mEditDate             :: Maybe Int
    , mMediaGroupId         :: Maybe Text
    , mAuthorSignature      :: Maybe Text
    , mText                 :: Maybe Text
  -- , mEntities :: Maybe [MessageEntity]
  -- , mAnimation :: Maybe Animation
    , mAudio                :: Maybe File
    , mDocument             :: Maybe File
    , mPhoto                :: Maybe [File]
    , mSticker              :: Maybe File
    , mVideo                :: Maybe File
    , mVideoNote            :: Maybe File
    , mVoice                :: Maybe File
    , mCaption              :: Maybe Text
  -- , mCaptionEntities :: Maybe [MessageEntity]
    , mContact              :: Maybe Contact
  -- , mDice :: Maybe Dice
  -- , mGame :: Maybe Game
    , mPoll                 :: Maybe Poll
  -- , mVenue                :: Maybe Venue
    , mLocation             :: Maybe Location
  -- , mNewChatMembers        :: Maybe [User]
  -- , mLeftChatMember        :: Maybe User
  -- , mNewChatTitle          :: Maybe Text
  -- , mNewChatPhoto :: Maybe [PhotoSize]
  -- , mDeleteChatPhoto       :: Maybe Bool
  -- , mGroupChatCreated      :: Maybe Bool
  -- , mSupergroupChatCreated :: Maybe Bool
  -- , mChannelChatCreated    :: Maybe Bool
  -- , mMigrateToChatId       :: Maybe Int
  -- , mMigrateFromChatId     :: Maybe Int
  -- , mPinnedMessage         :: Maybe Message
  -- , mInvoice :: Maybe Invoice
  -- , mSuccessfulPayment :: Maybe SuccessfulPayment
  -- , mConnectedWebsite      :: Maybe Text
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
      o .:? "audio" <*>
      o .:? "document" <*>
      o .:? "photo" <*>
      o .:? "sticker" <*>
      o .:? "video" <*>
      o .:? "video_note" <*>
      o .:? "voice" <*>
      o .:? "caption" <*>
      o .:? "contact" <*>
      o .:? "poll" <*>
      o .:? "location"

newtype File =
  File
    { file_id :: Text
    }
  deriving (Show, Generic, FromJSON)

data Contact =
  Contact
    { phone_number :: Text
    , first_name   :: Text
    }
  deriving (Show, Generic, FromJSON)

data Poll =
  Poll
    { pQuestion              :: Text
    , pOptions               :: [PollOption]
    , pIsClosed              :: Bool
    , pIsAnonymous           :: Bool
    , pType                  :: Text
    , pAllowsMultipleAnswers :: Bool
    , pCorrectOptionId       :: Maybe Int
    , pExplanation           :: Maybe Text
    , pExplanationEntities   :: Maybe [MessageEntity]
    , pOpenPeriod            :: Maybe Int
    , pCloseDate             :: Maybe Int
    }
  deriving (Show)

instance FromJSON Poll where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.Poll" $ \o ->
      Poll <$> o .: "question" <*> o .: "options" <*> o .: "is_closed" <*>
      o .: "is_anonymous" <*>
      o .: "type" <*>
      o .: "allows_multiple_answers" <*>
      o .:? "correct_option_id" <*>
      o .:? "explanation" <*>
      o .:? "explanation_entities" <*>
      o .:? "open_period" <*>
      o .:? "close_date"

newtype PollOption =
  PollOption
    { text :: Text
    }
  deriving (Show, Generic, FromJSON)

data MessageEntity =
  MessageEntity
    { meType     :: Text
    , meOffset   :: Int
    , meLength   :: Int
    , meUrl      :: Maybe Text
    , meUser     :: Maybe User
    , meLanguage :: Maybe Text
    }
  deriving (Show)

instance FromJSON MessageEntity where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.MessageEntity" $ \o ->
      MessageEntity <$> o .: "type" <*> o .: "offset" <*> o .: "length" <*>
      o .: "url" <*>
      o .: "user" <*>
      o .: "language"

instance ToJSON MessageEntity where
  toJSON o =
    object
      [ "type" .= meType o
      , "offset" .= meOffset o
      , "length" .= meLength o
      , "url" .= meUrl o
      , "user" .= meUser o
      , "language" .= meLanguage o
      ]

data Location =
  Location
    { longitude              :: Float
    , latitude               :: Float
    , horizontal_accuracy    :: Maybe Float
    , live_period            :: Maybe Int
    , heading                :: Maybe Int
    , proximity_alert_radius :: Maybe Int
    }
  deriving (Show)

instance FromJSON Location where
  parseJSON =
    withObject "FromJSON Bot.Telegram.Updates.Location" $ \o ->
      Location <$> o .: "longitude" <*> o .: "latitude" <*>
      o .:? "horizontal_accuracy" <*>
      o .:? "live_period" <*>
      o .:? "heading" <*>
      o .:? "proximity_alert_radius"
