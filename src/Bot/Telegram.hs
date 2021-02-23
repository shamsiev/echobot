{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram
  ( new
  , parseConfig
  ) where

import           Bot                               (Handle (Handle))
import           Bot.Telegram.Config
import qualified Bot.Telegram.HandleMessage        as HM
import qualified Bot.Telegram.HandleMessage.Help   as HMHelp
import qualified Bot.Telegram.HandleMessage.Repeat as HMRepeat
import qualified Bot.Telegram.HandleMessage.Text   as HMText
import           Bot.Telegram.Updates
import           Control.Lens                      ((&), (.~), (^.))
import           Control.Monad                     (forever)
import           Data.Aeson                        (KeyValue ((.=)),
                                                    eitherDecode,
                                                    eitherDecodeFileStrict,
                                                    encode, object)
import           Data.IORef                        (IORef, newIORef, readIORef,
                                                    writeIORef)
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text, unpack)
import qualified Logger
import           Network.Wreq                      (defaults, header, postWith,
                                                    responseBody,
                                                    responseStatus, statusCode)

type Token = Text

type Offset = Int

type Coutner = Int

type ChatId = Int

type Counters = [(ChatId, Coutner)]

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = do
  offset <- newIORef 0
  counters <- newIORef []
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
  _ <-
    (\h -> HM.handle h (cToken config) msg) =<<
    HMHelp.new hLogger (cHelpMessage config)
  _ <-
    (\h -> HM.handle h (cToken config) msg) =<<
    HMRepeat.new hLogger (cRepeatMessage config)
  handleUpdates config hLogger countersRef us
handleUpdates config hLogger countersRef ((callback_query -> Just cq):us) = do
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
