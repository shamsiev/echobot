{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.VideoNote
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (File (file_id), Message (mCaption, mFrom, mVideoNote),
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
  return $ Handle (sendVideoNote config hLogger counters)

sendVideoNote :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendVideoNote _ _ _ (mFrom -> Nothing) = return ()
sendVideoNote _ _ _ (mVideoNote -> Nothing) = return ()
sendVideoNote config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "video_note" .= (file_id . fromJust . mVideoNote) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/sendVideoNote"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendVideoNote"
      code -> Logger.warning hLogger (show code ++ " - sendVideoNote")
