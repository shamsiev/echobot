{-# LANGUAGE OverloadedStrings #-}

import qualified Logger
import qualified Logger.Test as Logger
import qualified Messenger
import qualified Messenger.Telegram as Telegram
import Network.HTTP.Simple
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe)

main :: IO ()
main =
  hspec $ do
    describe "Logger.debug" $ do
      it "minLevel = Debug" $ do
        h <- Logger.new Logger.Debug
        logger <- Logger.debug h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Info" $ do
        h <- Logger.new Logger.Info
        logger <- Logger.debug h "message"
        logger `shouldBe` Logger.NoLogger
      it "minLevel = Warning" $ do
        h <- Logger.new Logger.Warning
        logger <- Logger.debug h "message"
        logger `shouldBe` Logger.NoLogger
      it "minLevel = Error" $ do
        h <- Logger.new Logger.Error
        logger <- Logger.debug h "message"
        logger `shouldBe` Logger.NoLogger
--------------------------------------------------------------------------------
    describe "Logger.info" $ do
      it "minLevel = Debug" $ do
        h <- Logger.new Logger.Debug
        logger <- Logger.info h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Info" $ do
        h <- Logger.new Logger.Info
        logger <- Logger.info h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Warning" $ do
        h <- Logger.new Logger.Warning
        logger <- Logger.info h "message"
        logger `shouldBe` Logger.NoLogger
      it "minLevel = Error" $ do
        h <- Logger.new Logger.Error
        logger <- Logger.info h "message"
        logger `shouldBe` Logger.NoLogger
--------------------------------------------------------------------------------
    describe "Logger.warning" $ do
      it "minLevel = Debug" $ do
        h <- Logger.new Logger.Debug
        logger <- Logger.warning h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Info" $ do
        h <- Logger.new Logger.Info
        logger <- Logger.warning h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Warning" $ do
        h <- Logger.new Logger.Warning
        logger <- Logger.warning h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Error" $ do
        h <- Logger.new Logger.Error
        logger <- Logger.info h "message"
        logger `shouldBe` Logger.NoLogger
--------------------------------------------------------------------------------
    describe "Logger.error" $ do
      it "minLevel = Debug" $ do
        h <- Logger.new Logger.Debug
        logger <- Logger.error h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Info" $ do
        h <- Logger.new Logger.Info
        logger <- Logger.error h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Warning" $ do
        h <- Logger.new Logger.Warning
        logger <- Logger.error h "message"
        logger `shouldNotBe` Logger.NoLogger
      it "minLevel = Error" $ do
        h <- Logger.new Logger.Error
        logger <- Logger.error h "message"
        logger `shouldNotBe` Logger.NoLogger
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.callbackQueryToEvent" $ do
      it "returns UnknownEvent" $ do
        Telegram.callbackQueryToEvent
          (Telegram.CallbackQuery "query_id" (Telegram.Chat 12345) Nothing) `shouldBe`
          Messenger.UnknownEvent
      it "returns EventQuery" $ do
        Telegram.callbackQueryToEvent
          (Telegram.CallbackQuery
             "query_id"
             (Telegram.Chat 12345)
             (Just "query_data")) `shouldBe`
          Messenger.EventQuery 12345 "query_id" "query_data"
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventVoice" $ do
      it "returns Nothing" $ do
        Telegram.eventVoice
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaVoice without caption" $ do
        Telegram.eventVoice
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaVoice "file_id"))
      it "returns EventMedia of type MediaVoice with caption" $ do
        Telegram.eventVoice
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaVoice
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventAudio" $ do
      it "returns Nothing" $ do
        Telegram.eventAudio
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaAudio without caption" $ do
        Telegram.eventAudio
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaAudio "file_id"))
      it "returns EventMedia of type MediaAudio with caption" $ do
        Telegram.eventAudio
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaAudio
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventVideo" $ do
      it "returns Nothing" $ do
        Telegram.eventVideo
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaVideo without caption" $ do
        Telegram.eventVideo
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaVideo "file_id"))
      it "returns EventMedia of type MediaVideo with caption" $ do
        Telegram.eventVideo
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaVideo
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventPhoto" $ do
      it "returns Nothing" $ do
        Telegram.eventPhoto
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaPhoto without caption" $ do
        Telegram.eventPhoto
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just [Telegram.File "file_id"])
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaPhoto "file_id"))
      it "returns EventMedia of type MediaPhoto with caption" $ do
        Telegram.eventPhoto
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             Nothing
             Nothing
             (Just [Telegram.File "file_id"])
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaPhoto
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventDocument" $ do
      it "returns Nothing" $ do
        Telegram.eventDocument
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaDocument without caption" $ do
        Telegram.eventDocument
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  Nothing
                  Messenger.MediaDocument
                  "file_id"))
      it "returns EventMedia of type MediaDocument with caption" $ do
        Telegram.eventDocument
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaDocument
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventAnimation" $ do
      it "returns Nothing" $ do
        Telegram.eventAnimation
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaAnimation without caption" $ do
        Telegram.eventAnimation
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  Nothing
                  Messenger.MediaAnimation
                  "file_id"))
      it "returns EventMedia of type MediaAnimation with caption" $ do
        Telegram.eventAnimation
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia
                  (Just "caption")
                  Messenger.MediaAnimation
                  "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventSticker" $ do
      it "returns Nothing" $ do
        Telegram.eventSticker
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMedia of type MediaSticker" $ do
        Telegram.eventSticker
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaSticker "file_id"))
      it "returns EventMedia of type MediaSticker with caption ignored" $ do
        Telegram.eventSticker
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             (Just "caption")
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just
            (Messenger.EventMedia
               1234
               (Messenger.TelegramMedia Nothing Messenger.MediaSticker "file_id"))
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventMessage" $ do
      it "returns Nothing" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns Nothing ignoring /help command" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/help")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns Nothing ignoring /repeat command" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/repeat")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventMessage" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "message")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventMessage 1234 "message")
      it "returns EventMessage with caption ignored" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "message")
             (Just "caption")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventMessage 1234 "message")
      it "returns EventMessage with file ignored" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "message")
             Nothing
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventMessage 1234 "message")
      it "returns EventMessage with bot file and caption ignored" $ do
        Telegram.eventMessage
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "message")
             (Just "caption")
             (Just (Telegram.File "file_id"))
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventMessage 1234 "message")
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventHelpCommand" $ do
      it "returns Nothing" $ do
        Telegram.eventHelpCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns Nothing ignoring text" $ do
        Telegram.eventHelpCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "some text")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventHelpCommand" $ do
        Telegram.eventHelpCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/help")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventHelpCommand 1234)
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.eventRepeatCommand" $ do
      it "returns Nothing" $ do
        Telegram.eventRepeatCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns Nothing ignoring text" $ do
        Telegram.eventRepeatCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "some text")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Nothing
      it "returns EventHelpCommand" $ do
        Telegram.eventRepeatCommand
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/repeat")
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Just (Messenger.EventRepeatCommand 1234)
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.messageToEvent" $ do
      it "returns UnknownEvent" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing) `shouldBe`
          Messenger.UnknownEvent
      it "returns EventMessage" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "message")
             (Just "caption")
             (Just (Telegram.File "sticker"))
             (Just (Telegram.File "animation"))
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMessage 1234 "message"
      it "returns EventHelpCommand" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/help")
             (Just "caption")
             (Just (Telegram.File "sticker"))
             (Just (Telegram.File "animation"))
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventHelpCommand 1234
      it "returns EventRepeatCommand" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             (Just "/repeat")
             (Just "caption")
             (Just (Telegram.File "sticker"))
             (Just (Telegram.File "animation"))
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventRepeatCommand 1234
      it "returns EventMedia of type MediaSticker" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             (Just (Telegram.File "sticker"))
             (Just (Telegram.File "animation"))
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaSticker "sticker")
      it "returns EventMedia of type MediaAnimation" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "animation"))
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia
               Nothing
               Messenger.MediaAnimation
               "animation")
      it "returns EventMedia of type MediaDocument" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "document"))
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaDocument "document")
      it "returns EventMedia of type MediaPhoto" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just [Telegram.File "photo"])
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaPhoto "photo")
      it "returns EventMedia of type MediaVideo" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "video"))
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaVideo "video")
      it "returns EventMedia of type MediaAudio" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "audio"))
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaAudio "audio")
      it "returns EventMedia of type MediaVoice" $ do
        Telegram.messageToEvent
          (Telegram.Message
             (Telegram.Chat 1234)
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             Nothing
             (Just (Telegram.File "voice"))) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaVoice "voice")
--------------------------------------------------------------------------------
    describe "Messenger.Telegram.updateToEvent" $ do
      it "returns UnknownEvent" $ do
        Telegram.updateToEvent (Telegram.Update 0 Nothing Nothing) `shouldBe`
          Messenger.UnknownEvent
      it "returns EventMessage" $ do
        Telegram.updateToEvent
          (Telegram.Update
             0
             (Just $
              Telegram.Message
                (Telegram.Chat 1234)
                (Just "message")
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             Nothing) `shouldBe`
          Messenger.EventMessage 1234 "message"
      it "returns EventHelpCommand" $ do
        Telegram.updateToEvent
          (Telegram.Update
             0
             (Just $
              Telegram.Message
                (Telegram.Chat 1234)
                (Just "/help")
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             Nothing) `shouldBe`
          Messenger.EventHelpCommand 1234
      it "returns EventRepeatCommand" $ do
        Telegram.updateToEvent
          (Telegram.Update
             0
             (Just $
              Telegram.Message
                (Telegram.Chat 1234)
                (Just "/repeat")
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             Nothing) `shouldBe`
          Messenger.EventRepeatCommand 1234
      it "returns EventMedia" $ do
        Telegram.updateToEvent
          (Telegram.Update
             0
             (Just $
              Telegram.Message
                (Telegram.Chat 1234)
                Nothing
                Nothing
                (Just (Telegram.File "sticker"))
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing)
             Nothing) `shouldBe`
          Messenger.EventMedia
            1234
            (Messenger.TelegramMedia Nothing Messenger.MediaSticker "sticker")
      it "returns EventQuery" $ do
        Telegram.updateToEvent
          (Telegram.Update
             0
             Nothing
             (Just $
              Telegram.CallbackQuery
                "query_id"
                (Telegram.Chat 1234)
                (Just "query_data"))) `shouldBe`
          Messenger.EventQuery 1234 "query_id" "query_data"
