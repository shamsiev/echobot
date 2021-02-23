{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Location
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (Location (heading, horizontal_accuracy, latitude, live_period, longitude, proximity_alert_radius),
                                             Message (mFrom, mLocation),
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
  return $ Handle (sendLocation config hLogger counters)

sendLocation :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendLocation _ _ _ (mFrom -> Nothing) = return ()
sendLocation _ _ _ (mLocation -> Nothing) = return ()
sendLocation config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "latitude" .= (latitude . fromJust . mLocation) message
          , "longitude" .= (longitude . fromJust . mLocation) message
          , "horizontal_accuracy" .=
            (horizontal_accuracy . fromJust . mLocation) message
          , "live_period" .= (live_period . fromJust . mLocation) message
          , "heading" .= (heading . fromJust . mLocation) message
          , "proximity_alert_radius" .=
            (proximity_alert_radius . fromJust . mLocation) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/sendLocation"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendLocation"
      code -> Logger.warning hLogger (show code ++ " - sendLocation")
