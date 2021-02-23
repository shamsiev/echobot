{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Contact
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (Contact (first_name, phone_number),
                                             File (file_id),
                                             Message (mCaption, mContact, mFrom),
                                             User (uId))
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
  return $ Handle (sendContact config hLogger counters)

sendContact :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendContact _ _ _ (mFrom -> Nothing) = return ()
sendContact _ _ _ (mContact -> Nothing) = return ()
sendContact config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "phone_number" .= (phone_number . fromJust . mContact) message
          , "first_name" .= (first_name . fromJust . mContact) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/sendContact"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendContact"
      code -> Logger.warning hLogger (show code ++ " - sendContact")
