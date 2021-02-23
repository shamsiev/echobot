{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Help
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cHelpMessage, cToken))
import           Bot.Telegram.HandleMessage (Handle (Handle))
import           Bot.Telegram.Updates       (Message (mFrom, mText), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (KeyValue ((.=)), object)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = return $ Handle (sendHelpMessage config hLogger)

sendHelpMessage :: Config -> Logger.Handle -> Message -> IO ()
sendHelpMessage _ _ (mFrom -> Nothing) = return ()
sendHelpMessage config hLogger message@(mText -> Just "/help") = do
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "text" .= cHelpMessage config
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/sendMessage"
  response <- postWith options address requestObject
  case response ^. responseStatus . statusCode of
    200  -> Logger.info hLogger "200 - sendMessage"
    code -> Logger.warning hLogger (show code ++ " - sendMessage")
sendHelpMessage _ _ _ = return ()
