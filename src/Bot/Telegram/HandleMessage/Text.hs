{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Text
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (Message (mFrom, mText), User (uId))
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad              (replicateM_)
import           Data.Aeson                 (KeyValue ((.=)), object)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Text                  (unpack)
import qualified Logger
import           Network.Wreq               (defaults, header, postWith,
                                             responseStatus, statusCode)

new :: Config -> Logger.Handle -> Counters -> IO Handle
new config hLogger counters =
  return $ Handle (sendMessage config hLogger counters)

sendMessage :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendMessage _ _ _ (mFrom -> Nothing) = return ()
sendMessage _ _ _ (mText -> Nothing) = return ()
sendMessage _ _ _ (mText -> Just "/help") = return ()
sendMessage _ _ _ (mText -> Just "/repeat") = return ()
sendMessage config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "text" .= (fromJust . mText) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/sendMessage"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendMessage"
      code -> Logger.warning hLogger (show code ++ " - sendMessage")
