{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Repeat
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatMessage, cToken))
import           Bot.Telegram.HandleMessage (Handle (Handle))
import           Bot.Telegram.Updates       (Message (mFrom, mText), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Data.Aeson                 (KeyValue ((.=)), ToJSON (toJSON),
                                             object)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

new :: Config -> Logger.Handle -> IO Handle
new config hLogger = return $ Handle (sendMessage config hLogger)

sendMessage :: Config -> Logger.Handle -> Message -> IO ()
sendMessage _ _ (mFrom -> Nothing) = return ()
sendMessage config hLogger message@(mText -> Just "/repeat") = do
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "text" .= cRepeatMessage config
          , "reply_markup" .= keyboard
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
sendMessage _ _ _ = return ()

keyboard :: Keyboard
keyboard =
  Keyboard
    [ [Button "1" "1"]
    , [Button "2" "2"]
    , [Button "3" "3"]
    , [Button "4" "4"]
    , [Button "5" "5"]
    ]

newtype Keyboard =
  Keyboard
    { kButtons :: [[Button]]
    }
  deriving (Show)

instance ToJSON Keyboard where
  toJSON o = object ["inline_keyboard" .= kButtons o]

data Button =
  Button
    { bText         :: Text
    , bCallbackData :: Text
    }
  deriving (Show)

instance ToJSON Button where
  toJSON o = object ["text" .= bText o, "callback_data" .= bCallbackData o]
