{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Text
  ( new
  ) where

import           Bot.Telegram.HandleMessage (Handle (Handle), Token)
import           Bot.Telegram.Updates       (Message (mFrom, mText), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (KeyValue ((.=)), object)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

new :: Logger.Handle -> IO Handle
new hLogger = return $ Handle (sendTextMessage hLogger)

sendTextMessage :: Logger.Handle -> Token -> Message -> IO ()
sendTextMessage _ _ (mFrom -> Nothing) = return ()
sendTextMessage _ _ (mText -> Nothing) = return ()
sendTextMessage _ _ (mText -> Just "/help") = return ()
sendTextMessage _ _ (mText -> Just "/repeat") = return ()
sendTextMessage hLogger token message = do
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "text" .= (fromJust . mText) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address = "https://api.telegram.org/bot" ++ unpack token ++ "/sendMessage"
  response <- postWith options address requestObject
  case response ^. responseStatus . statusCode of
    200  -> Logger.info hLogger "200 - sendMessage"
    code -> Logger.warning hLogger (show code ++ " - sendMessage")
