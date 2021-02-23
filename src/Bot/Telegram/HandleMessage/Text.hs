{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram.HandleMessage.Text
  ( new
  ) where

import           Bot.Telegram.HandleMessage (Handle (Handle), Token)
import           Bot.Telegram.Updates       (Message (..), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (KeyValue ((.=)), object)
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Text                  (unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

new :: Logger.Handle -> IO Handle
new hLogger = return $ Handle (textMessage hLogger)

textMessage :: Logger.Handle -> Token -> Message -> IO ()
textMessage hLogger token Message {..}
  | isNothing mText || isNothing mFrom = return ()
  | otherwise = do
    let requestObject =
          object ["chat_id" .= (uId . fromJust) mFrom, "text" .= fromJust mText]
    let options =
          defaults & header "Content-Type" .~
          ["application/json; charset=utf-8"]
    let address =
          "https://api.telegram.org/bot" ++ unpack token ++ "/sendMessage"
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendMessage"
      code -> Logger.warning hLogger (show code ++ " - sendMessage")
