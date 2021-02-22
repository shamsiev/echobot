{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE RecordWildCards   #-}
module Bot.Telegram
  ( new
  , parseConfig
  ) where

import           Bot                       (Handle (Handle))
import           Bot.Telegram.Config       (Config (cToken))
import           Bot.Telegram.Updates      (Updates)
import           Control.Monad             (forever)
import           Data.Aeson                (KeyValue ((.=)), eitherDecode,
                                            eitherDecodeFileStrict, encode,
                                            object)
import           Data.IORef                (IORef, newIORef)
import           Data.Text                 (unpack)
import qualified Logger
import           Network.HTTP.Client       (Request (method, requestBody, requestHeaders),
                                            RequestBody (RequestBodyLBS),
                                            Response (responseBody, responseStatus),
                                            defaultManagerSettings, httpLbs,
                                            newManager, parseRequest)
import           Network.HTTP.Types.Status (Status (statusCode))

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
telegram config hLogger offsetRef countersRef
  -- updates <-
  -- handleMessage text updates
 = do
  return ()

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config

getUpdates :: Config -> Logger.Handle -> Offset -> IO Updates
getUpdates config hLogger offset = do
  manager <- newManager defaultManagerSettings
  let requestObject = object ["offset" .= offset]
  initialRequest <-
    parseRequest $
    "https://api.telegram.org/bot" ++ unpack (cToken config) ++ "/getUpdates"
  let request =
        initialRequest
          { method = "POST"
          , requestBody = RequestBodyLBS $ encode requestObject
          , requestHeaders =
              [("Content-Type", "application/json; charset=utf-8")]
          }
  response <- httpLbs request manager
  case statusCode $ responseStatus response of
    200  -> Logger.info hLogger "200 - getUpdates"
    code -> Logger.warning hLogger $ show code ++ " - getUpdates"
  let parseUpdates =
        eitherDecode (responseBody response) :: Either String Updates
  either fail return parseUpdates
