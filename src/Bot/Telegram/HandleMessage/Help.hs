{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Help
  ( new
  , HelpMessage
  ) where

import           Bot.Telegram.HandleMessage (Handle (Handle), Token)
import           Bot.Telegram.Updates       (Message (mFrom, mText), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (KeyValue ((.=)), object)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

type HelpMessage = Text

new :: Logger.Handle -> HelpMessage -> IO Handle
new hLogger helpMessage = return $ Handle (sendHelpMessage hLogger helpMessage)

sendHelpMessage :: Logger.Handle -> HelpMessage -> Token -> Message -> IO ()
sendHelpMessage _ _ _ (mFrom -> Nothing) = return ()
sendHelpMessage hLogger helpMessage token message@(mText -> Just "/help") = do
  let requestObject =
        object
          ["chat_id" .= (uId . fromJust . mFrom) message, "text" .= helpMessage]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address = "https://api.telegram.org/bot" ++ unpack token ++ "/sendMessage"
  response <- postWith options address requestObject
  case response ^. responseStatus . statusCode of
    200  -> Logger.info hLogger "200 - sendMessage"
    code -> Logger.warning hLogger (show code ++ " - sendMessage")
sendHelpMessage _ _ _ _ = return ()
