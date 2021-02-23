{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Bot.Telegram.HandleCallbackQuery
  ( Handle(..)
  , new
  ) where

import           Bot.Telegram.Config  (Config (cToken))
import           Bot.Telegram.Updates (CallbackQuery (cqData, cqFrom, cqId),
                                       User (uId))
import           Control.Lens         ((&), (.~), (^.))
import           Data.Aeson           (KeyValue ((.=)), object)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import           Data.Text            (Text, unpack)
import qualified Logger
import           Network.Wreq         (defaults, header, postWith,
                                       responseStatus, statusCode)

type ChatId = Int

type Counter = Int

type Counters = M.Map ChatId Counter

newtype Handle =
  Handle
    { handle :: CallbackQuery -> IO Counters
    }

new :: Config -> Logger.Handle -> Counters -> IO Handle
new config hLogger counters =
  return $ Handle (handleCallbackQuery config hLogger counters)

handleCallbackQuery ::
     Config -> Logger.Handle -> Counters -> CallbackQuery -> IO Counters
handleCallbackQuery _ _ counters (cqData -> Nothing) = return counters
handleCallbackQuery config hLogger counters cq = do
  let newCounter = read (unpack $ fromJust $ cqData cq) :: Int
  let chatId = uId $ cqFrom cq
  let requestObject =
        object
          [ "callback_query_id" .= cqId cq
          , "text" .= ("Set repeat counter to " ++ show newCounter)
          ]
  let options =
        defaults & header "Content-Type" .~ ["application/json; charset=utf-8"]
  let address =
        "https://api.telegram.org/bot" ++ unpack (cToken config) ++
        "/answerCallbackQuery"
  response <- postWith options address requestObject
  case response ^. responseStatus . statusCode of
    200  -> Logger.info hLogger "200 - answerCallbackQuery"
    code -> Logger.warning hLogger (show code ++ " - answerCallbackQuery")
  return $ M.insert chatId newCounter counters
