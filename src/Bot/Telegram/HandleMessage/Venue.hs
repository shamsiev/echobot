{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Venue
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (Location (latitude),
                                             Message (mFrom, mVenue),
                                             User (uId),
                                             Venue (location, vAddress, vFoursquareId, vFoursquareType, vGooglePlaceId, vGooglePlaceType, vTitle))
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
  return $ Handle (sendVenue config hLogger counters)

sendVenue :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendVenue _ _ _ (mFrom -> Nothing) = return ()
sendVenue _ _ _ (mVenue -> Nothing) = return ()
sendVenue config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "latitude" .= (latitude . location . fromJust . mVenue) message
          , "longitude" .= (latitude . location . fromJust . mVenue) message
          , "title" .= (vTitle . fromJust . mVenue) message
          , "address" .= (vAddress . fromJust . mVenue) message
          , "foursquare_id" .= (vFoursquareId . fromJust . mVenue) message
          , "foursquare_type" .= (vFoursquareType . fromJust . mVenue) message
          , "google_place_id" .= (vGooglePlaceId . fromJust . mVenue) message
          , "google_place_type" .=
            (vGooglePlaceType . fromJust . mVenue) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++ "/sendVenue"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendVenue"
      code -> Logger.warning hLogger (show code ++ " - sendVenue")
