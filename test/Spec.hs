{-# LANGUAGE OverloadedStrings #-}

import qualified Logger
import qualified Logger.Test as Logger
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
