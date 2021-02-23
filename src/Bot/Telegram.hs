{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram
  ( new
  , parseConfig
  ) where

import           Bot                                  (Handle (Handle))
import           Bot.Telegram.Config                  (Config (cToken))
import qualified Bot.Telegram.HandleCallbackQuery     as HCQ
import qualified Bot.Telegram.HandleMessage           as HM
import qualified Bot.Telegram.HandleMessage.Audio     as HMAudio
import qualified Bot.Telegram.HandleMessage.Contact   as HMContact
import qualified Bot.Telegram.HandleMessage.Document  as HMDocument
import qualified Bot.Telegram.HandleMessage.Help      as HMHelp
import qualified Bot.Telegram.HandleMessage.Location  as HMLocation
import qualified Bot.Telegram.HandleMessage.Photo     as HMPhoto
import qualified Bot.Telegram.HandleMessage.Poll      as HMPoll
import qualified Bot.Telegram.HandleMessage.Repeat    as HMRepeat
import qualified Bot.Telegram.HandleMessage.Sticker   as HMSticker
import qualified Bot.Telegram.HandleMessage.Text      as HMText
import qualified Bot.Telegram.HandleMessage.Video     as HMVideo
import qualified Bot.Telegram.HandleMessage.VideoNote as HMVideoNote
import qualified Bot.Telegram.HandleMessage.Voice     as HMVoice
import           Bot.Telegram.Updates                 (Update (callback_query, message, update_id),
                                                       Updates (result))
import           Control.Lens                         ((&), (.~), (^.))
import           Control.Monad                        (forever)
import           Data.Aeson                           (KeyValue ((.=)),
                                                       eitherDecode,
                                                       eitherDecodeFileStrict,
                                                       encode, object)
import           Data.IORef                           (IORef, newIORef,
                                                       readIORef, writeIORef)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromJust, fromMaybe,
                                                       isJust)
import           Data.Text                            (Text, unpack)
import qualified Logger
import           Network.Wreq                         (defaults, header,
                                                       postWith, responseBody,
                                                       responseStatus,
                                                       statusCode)

type Token = Text

type Offset = Int

type ChatId = Int

type Counter = Int

type Counters = M.Map ChatId Counter

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = do
  offset <- newIORef 0
  counters <- newIORef M.empty
  return $ Handle $ forever $ telegram config hLogger offset counters

telegram :: Config -> Logger.Handle -> IORef Offset -> IORef Counters -> IO ()
telegram config hLogger offsetRef countersRef = do
  offset <- readIORef offsetRef
  updates <- getUpdates hLogger (cToken config) offset
  newCountersRef <- handleUpdates config hLogger countersRef (result updates)
  writeIORef countersRef =<< readIORef newCountersRef
  case map update_id (result updates) of
    []      -> writeIORef offsetRef offset
    offsets -> writeIORef offsetRef (maximum offsets + 1)

handleUpdates ::
     Config
  -> Logger.Handle
  -> IORef Counters
  -> [Update]
  -> IO (IORef Counters)
handleUpdates _ _ countersRef [] = return countersRef
handleUpdates config hLogger countersRef ((message -> Just msg):us) = do
  counters <- readIORef countersRef
  _ <- (`HM.handle` msg) =<< HMHelp.new config hLogger
  _ <- (`HM.handle` msg) =<< HMRepeat.new config hLogger
  _ <- (`HM.handle` msg) =<< HMText.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMAudio.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMDocument.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMPhoto.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMSticker.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMVideo.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMVideoNote.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMVoice.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMContact.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMPoll.new config hLogger counters
  _ <- (`HM.handle` msg) =<< HMLocation.new config hLogger counters
  handleUpdates config hLogger countersRef us
handleUpdates config hLogger countersRef ((callback_query -> Just cq):us) = do
  counters <- readIORef countersRef
  newCounters <- (`HCQ.handle` cq) =<< HCQ.new config hLogger counters
  writeIORef countersRef newCounters
  handleUpdates config hLogger countersRef us
handleUpdates config hLogger countersRef (_:us) =
  handleUpdates config hLogger countersRef us

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config

getUpdates :: Logger.Handle -> Token -> Offset -> IO Updates
getUpdates hLogger token offset = do
  let requestObject = object ["offset" .= offset]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address = "https://api.telegram.org/bot" ++ unpack token ++ "/getUpdates"
  response <- postWith options address requestObject
  case response ^. responseStatus . statusCode of
    200  -> Logger.info hLogger "200 - getUpdates"
    code -> Logger.warning hLogger (show code ++ " - getUpdates")
  either
    fail
    return
    (eitherDecode (response ^. responseBody) :: Either String Updates)
