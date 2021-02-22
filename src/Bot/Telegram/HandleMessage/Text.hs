{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram.HandleMessage.Text
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cToken))
import           Bot.Telegram.HandleMessage (Handle (Handle))
import           Bot.Telegram.Updates       (Message (..), User (uId))
import           Data.Aeson                 (KeyValue ((.=)), encode, object)
import           Data.Maybe                 (fromJust, isJust, isNothing)
import           Data.Text                  (unpack)
import qualified Logger
import           Network.HTTP.Client        (Request (method, requestBody, requestHeaders),
                                             RequestBody (RequestBodyLBS),
                                             Response (responseStatus),
                                             defaultManagerSettings, httpLbs,
                                             newManager, parseRequest)
import           Network.HTTP.Types.Status  (Status (statusCode))

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = return $ Handle (textMessage config hLogger)

textMessage :: Config -> Logger.Handle -> Message -> IO ()
textMessage config hLogger Message {..}
  | isNothing mText || isNothing mFrom = return ()
  | otherwise = do
    manager <- newManager defaultManagerSettings
    let requestObject =
          object ["chat_id" .= (uId . fromJust) mFrom, "text" .= fromJust mText]
    initialRequest <-
      parseRequest $
      "https://api.telegram.org/bot" ++ unpack (cToken config) ++ "/sendMessage"
    let request =
          initialRequest
            { method = "POST"
            , requestBody = RequestBodyLBS $ encode requestObject
            , requestHeaders =
                [("Content-Type", "application/json; charset=utf-8")]
            }
    response <- httpLbs request manager
    case statusCode $ responseStatus response of
      200  -> Logger.info hLogger "200 - sendMessage"
      code -> Logger.warning hLogger $ show code ++ " - sendMessage"
    return ()
