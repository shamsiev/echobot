{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleMessage.Poll
  ( new
  ) where

import           Bot.Telegram.Config        (Config (cRepeatCount, cToken))
import           Bot.Telegram.HandleMessage (Counters, Handle (Handle))
import           Bot.Telegram.Updates       (Message (mFrom, mPoll), Poll (pAllowsMultipleAnswers, pCloseDate, pCorrectOptionId, pExplanation, pExplanationEntities, pIsAnonymous, pIsClosed, pOpenPeriod, pOptions, pQuestion, pType),
                                             PollOption (text), User (uId))
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
new config hLogger counters = return $ Handle (sendPoll config hLogger counters)

sendPoll :: Config -> Logger.Handle -> Counters -> Message -> IO ()
sendPoll _ _ _ (mFrom -> Nothing) = return ()
sendPoll _ _ _ (mPoll -> Nothing) = return ()
sendPoll config hLogger counters message = do
  let counter =
        fromMaybe
          (cRepeatCount config)
          (M.lookup (uId $ fromJust $ mFrom message) counters)
  let requestObject =
        object
          [ "chat_id" .= (uId . fromJust . mFrom) message
          , "question" .= (pQuestion . fromJust . mPoll) message
          , "options" .= (map text . pOptions . fromJust . mPoll) message
          , "is_anonymous" .= (pIsAnonymous . fromJust . mPoll) message
          , "type" .= (pType . fromJust . mPoll) message
          , "allows_multiple_answers" .=
            (pAllowsMultipleAnswers . fromJust . mPoll) message
          , "correct_option_id" .= (pCorrectOptionId . fromJust . mPoll) message
          , "explanation" .= (pExplanation . fromJust . mPoll) message
          , "explanation_entities" .=
            (pExplanationEntities . fromJust . mPoll) message
          , "open_period" .= (pOpenPeriod . fromJust . mPoll) message
          , "close_date" .= (pCloseDate . fromJust . mPoll) message
          , "is_closed" .= (pIsClosed . fromJust . mPoll) message
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++ "/sendPoll"
  replicateM_ counter $ do
    response <- postWith options address requestObject
    case response ^. responseStatus . statusCode of
      200  -> Logger.info hLogger "200 - sendPoll"
      code -> Logger.warning hLogger (show code ++ " - sendPoll")
