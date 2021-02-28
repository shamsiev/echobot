{-# LANGUAGE OverloadedStrings #-}

import Bot
  ( Event(EventMedia, EventMessage, EventQuery)
  , Media(MediaAnimation, MediaAudio, MediaDocument, MediaPhoto,
      MediaSticker, MediaVideo, MediaVoice)
  )
import qualified Bot.Telegram
import qualified Bot.Telegram.Internal
import qualified Bot.VK.Internal
import Data.Aeson (KeyValue((.=)), object)
import Data.Text (Text)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe)

main :: IO ()
main =
  hspec $ do
    test1
    test2
    test3
    test4
    test5

--------------------------------------------------------------------------------
test1 :: SpecWith ()
test1 =
  describe "Bot.Telegram.Internal.messageToEvent" $ do
    it "returns EventMessage" $ do
      Bot.Telegram.Internal.messageToEvent textMessage `shouldBe`
        Just (EventMessage 1337 "text")
    it "returns EventMedia MediaSticker" $ do
      Bot.Telegram.Internal.messageToEvent stickerMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaSticker "file"])
    it "returns EventMedia MediaAnimation" $ do
      Bot.Telegram.Internal.messageToEvent animationMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaAnimation "file" "caption"])
    it "returns EventMedia MediaDocument" $ do
      Bot.Telegram.Internal.messageToEvent documentMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaDocument "file" "caption"])
    it "returns EventMedia MediaPhoto" $ do
      Bot.Telegram.Internal.messageToEvent photoMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaPhoto "file" "caption"])
    it "returns EventMedia MediaVideo" $ do
      Bot.Telegram.Internal.messageToEvent videoMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaVideo "file" "caption"])
    it "returns EventMedia MediaAudio" $ do
      Bot.Telegram.Internal.messageToEvent audioMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaAudio "file" "caption"])
    it "returns EventMedia MediaVoice" $ do
      Bot.Telegram.Internal.messageToEvent voiceMessage `shouldBe`
        Just (EventMedia 1337 "" [MediaVoice "file" "caption"])
    it "returns Nothing" $ do
      Bot.Telegram.Internal.messageToEvent invalidMessage `shouldBe` Nothing

textMessage :: Bot.Telegram.Internal.Message
textMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Just "text"
    , Bot.Telegram.Internal.mCaption = Nothing
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

stickerMessage :: Bot.Telegram.Internal.Message
stickerMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Nothing
    , Bot.Telegram.Internal.mSticker = Just (Bot.Telegram.Internal.File "file")
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

animationMessage :: Bot.Telegram.Internal.Message
animationMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation =
        Just (Bot.Telegram.Internal.File "file")
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

documentMessage :: Bot.Telegram.Internal.Message
documentMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Just (Bot.Telegram.Internal.File "file")
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

photoMessage :: Bot.Telegram.Internal.Message
photoMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Just [Bot.Telegram.Internal.File "file"]
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

videoMessage :: Bot.Telegram.Internal.Message
videoMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Just (Bot.Telegram.Internal.File "file")
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

audioMessage :: Bot.Telegram.Internal.Message
audioMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Just (Bot.Telegram.Internal.File "file")
    , Bot.Telegram.Internal.mVoice = Nothing
    }

voiceMessage :: Bot.Telegram.Internal.Message
voiceMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Just "caption"
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Just (Bot.Telegram.Internal.File "file")
    }

invalidMessage :: Bot.Telegram.Internal.Message
invalidMessage =
  Bot.Telegram.Internal.Message
    { Bot.Telegram.Internal.mChat = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.mText = Nothing
    , Bot.Telegram.Internal.mCaption = Nothing
    , Bot.Telegram.Internal.mSticker = Nothing
    , Bot.Telegram.Internal.mAnimation = Nothing
    , Bot.Telegram.Internal.mDocument = Nothing
    , Bot.Telegram.Internal.mPhoto = Nothing
    , Bot.Telegram.Internal.mVideo = Nothing
    , Bot.Telegram.Internal.mAudio = Nothing
    , Bot.Telegram.Internal.mVoice = Nothing
    }

--------------------------------------------------------------------------------
test2 :: SpecWith ()
test2 =
  describe "Bot.Telegram.Internal.queryToEvent" $ do
    it "returns EventQuery" $ do
      Bot.Telegram.Internal.queryToEvent queryMessage `shouldBe`
        Just (EventQuery 1337 "query_id" "query_data")
    it "returns Nothing" $ do
      Bot.Telegram.Internal.queryToEvent invalidQuery `shouldBe` Nothing

queryMessage :: Bot.Telegram.Internal.Query
queryMessage =
  Bot.Telegram.Internal.Query
    { Bot.Telegram.Internal.qId = "query_id"
    , Bot.Telegram.Internal.qFrom = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.qData = Just "query_data"
    }

invalidQuery :: Bot.Telegram.Internal.Query
invalidQuery =
  Bot.Telegram.Internal.Query
    { Bot.Telegram.Internal.qId = "1337"
    , Bot.Telegram.Internal.qFrom = Bot.Telegram.Internal.Chat 1337
    , Bot.Telegram.Internal.qData = Nothing
    }

--------------------------------------------------------------------------------
test3 :: SpecWith ()
test3 =
  describe "Bot.Telegram.Internal.updateToEvent" $ do
    it "returns EventMessage" $ do
      Bot.Telegram.Internal.updateToEvent messageUpdate `shouldBe`
        Just (EventMessage 1337 "text")
    it "returns EventQuery" $ do
      Bot.Telegram.Internal.updateToEvent queryUpdate `shouldBe`
        Just (EventQuery 1337 "query_id" "query_data")
    it "returns Nothing" $ do
      Bot.Telegram.Internal.updateToEvent invalidUpdate `shouldBe` Nothing

messageUpdate :: Bot.Telegram.Internal.Update
messageUpdate = Bot.Telegram.Internal.Update 8888 (Just textMessage) Nothing

queryUpdate :: Bot.Telegram.Internal.Update
queryUpdate = Bot.Telegram.Internal.Update 8888 Nothing (Just queryMessage)

invalidUpdate :: Bot.Telegram.Internal.Update
invalidUpdate = Bot.Telegram.Internal.Update 8888 Nothing Nothing

--------------------------------------------------------------------------------
test4 :: SpecWith ()
test4 =
  describe "Bot.Telegram.methodNameFromMedia" $ do
    it "converts MediaSticker" $ do
      Bot.Telegram.methodNameFromMedia (MediaSticker "") `shouldBe`
        "/sendSticker"
    it "converts MediaAnimation" $ do
      Bot.Telegram.methodNameFromMedia (MediaAnimation "" "") `shouldBe`
        "/sendAnimation"
    it "converts MediaDocument" $ do
      Bot.Telegram.methodNameFromMedia (MediaDocument "" "") `shouldBe`
        "/sendDocument"
    it "converts MediaPhoto" $ do
      Bot.Telegram.methodNameFromMedia (MediaPhoto "" "") `shouldBe`
        "/sendPhoto"
    it "converts MediaVideo" $ do
      Bot.Telegram.methodNameFromMedia (MediaVideo "" "") `shouldBe`
        "/sendVideo"
    it "converts MediaAudio" $ do
      Bot.Telegram.methodNameFromMedia (MediaAudio "" "") `shouldBe`
        "/sendAudio"
    it "converts MediaVoice" $ do
      Bot.Telegram.methodNameFromMedia (MediaVoice "" "") `shouldBe`
        "/sendVoice"

--------------------------------------------------------------------------------
test5 :: SpecWith ()
test5 =
  describe "Bot.Telegram.jsonFromMedia" $ do
    it "converts MediaSticker" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaSticker "sticker") `shouldBe`
        object ["chat_id" .= (1337 :: Int), "sticker" .= ("sticker" :: Text)]
    it "converts MediaAnimation" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaAnimation "animation" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "animation" .= ("animation" :: Text)
          , "caption" .= ("caption" :: Text)
          ]
    it "converts MediaDocument" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaDocument "document" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "document" .= ("document" :: Text)
          , "caption" .= ("caption" :: Text)
          ]
    it "converts MediaPhoto" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaPhoto "photo" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "photo" .= ("photo" :: Text)
          , "caption" .= ("caption" :: Text)
          ]
    it "converts MediaVideo" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaVideo "video" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "video" .= ("video" :: Text)
          , "caption" .= ("caption" :: Text)
          ]
    it "converts MediaAudio" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaAudio "audio" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "audio" .= ("audio" :: Text)
          , "caption" .= ("caption" :: Text)
          ]
    it "converts MediaVoice" $ do
      Bot.Telegram.jsonFromMedia 1337 (MediaVoice "voice" "caption") `shouldBe`
        object
          [ "chat_id" .= (1337 :: Int)
          , "voice" .= ("voice" :: Text)
          , "caption" .= ("caption" :: Text)
          ]

--------------------------------------------------------------------------------
test6 :: SpecWith ()
test6 =
  describe "Bot.VK.Internal.attachmentToMedia" $ do
    it "reads MediaPhoto without key" $ do
      Bot.VK.Internal.attachmentToMedia photoAttachment `shouldBe`
        Just (MediaPhoto "photo1337_7331" "")
    it "reads MediaPhoto with key" $ do
      Bot.VK.Internal.attachmentToMedia photoAttachment' `shouldBe`
        Just (MediaPhoto "photo1337_7331_key" "")
    it "reads MediaAudio without key" $ do
      Bot.VK.Internal.attachmentToMedia audioAttachment `shouldBe`
        Just (MediaAudio "audio1337_7331" "")
    it "reads MediaAudio with key" $ do
      Bot.VK.Internal.attachmentToMedia audioAttachment' `shouldBe`
        Just (MediaAudio "audio1337_7331_key" "")
    it "reads MediaDocument without key" $ do
      Bot.VK.Internal.attachmentToMedia docAttachment `shouldBe`
        Just (MediaDocument "doc1337_7331" "")
    it "reads MediaDucoment with key" $ do
      Bot.VK.Internal.attachmentToMedia docAttachment' `shouldBe`
        Just (MediaDocument "doc1337_7331_key" "")
    it "reads MediaVideo without key" $ do
      Bot.VK.Internal.attachmentToMedia videoAttachment `shouldBe`
        Just (MediaVideo "video1337_7331" "")
    it "reads MediaVideo with key" $ do
      Bot.VK.Internal.attachmentToMedia videoAttachment' `shouldBe`
        Just (MediaVideo "video1337_7331_key" "")
    it "reads MediaSticker" $ do
      Bot.VK.Internal.attachmentToMedia stickerAttachment `shouldBe`
        Just (MediaSticker "1337")
    it "rejects InvalidMedia" $ do
      Bot.VK.Internal.attachmentToMedia invalidAttachment `shouldBe` Nothing

photoAttachment :: Bot.VK.Internal.Attachment
photoAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aPhoto =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Nothing
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

photoAttachment' :: Bot.VK.Internal.Attachment
photoAttachment' =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aPhoto =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Just "key"
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

audioAttachment :: Bot.VK.Internal.Attachment
audioAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aAudio =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Nothing
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

audioAttachment' :: Bot.VK.Internal.Attachment
audioAttachment' =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aAudio =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Just "key"
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

docAttachment :: Bot.VK.Internal.Attachment
docAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aDocument =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Nothing
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

docAttachment' :: Bot.VK.Internal.Attachment
docAttachment' =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aDocument =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Just "key"
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aVideo = Nothing
    }

videoAttachment :: Bot.VK.Internal.Attachment
videoAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aVideo =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Nothing
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    }

videoAttachment' :: Bot.VK.Internal.Attachment
videoAttachment' =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aVideo =
        Just
          Bot.VK.Internal.File
            { Bot.VK.Internal.fAccessKey = Just "key"
            , Bot.VK.Internal.fId = 1337
            , Bot.VK.Internal.fOwnerId = 7331
            }
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    }

stickerAttachment :: Bot.VK.Internal.Attachment
stickerAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aVideo = Nothing
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Just $ Bot.VK.Internal.Sticker 1337
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    }

invalidAttachment :: Bot.VK.Internal.Attachment
invalidAttachment =
  Bot.VK.Internal.Attachment
    { Bot.VK.Internal.aVideo = Nothing
    , Bot.VK.Internal.aPhoto = Nothing
    , Bot.VK.Internal.aSticker = Nothing
    , Bot.VK.Internal.aAudio = Nothing
    , Bot.VK.Internal.aDocument = Nothing
    }
