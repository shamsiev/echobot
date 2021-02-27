{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram where

import Bot
import Bot.Telegram.Internal
import Data.Aeson (KeyValue((.=)),eitherDecode,object)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef (IORef,readIORef,writeIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text,pack,unpack)
import qualified Logger
import qualified Web
import Control.Monad (replicateM_)
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
type Token = Text

type Timeout = Int

type Offset = Int

type Counter = Int

type Counters = M.Map ChatId Counter

--------------------------------------------------------------------------------
data IHandle =
    IHandle
    { iToken :: Token
    , iHelpMessage :: Text
    , iRepeatMessage :: Text
    , iDefaultRepeat :: Counter
    , iTimeout :: Timeout
    , iOffset :: IORef Offset
    , iCounters :: IORef Counters
    , iLogger :: Logger.Handle
    }

--------------------------------------------------------------------------------
tgGetEvents :: IHandle -> IO [Event]
tgGetEvents IHandle {..} = do
    offset <- readIORef iOffset
    Logger.debug iLogger
        $ "Telegram: Current update offset: " <> pack (show offset)
    Logger.info iLogger "Telegram: Getting updates..."
    let address =
            "https://api.telegram.org/bot" ++ unpack iToken ++ "/getUpdates"
    let json =
            object
                [ "offset" .= pack (show offset)
                , "timeout" .= pack (show iTimeout)]
    (status,body) <- Web.sendJSON address json
    case status of
        200 -> do
            Logger.info iLogger "Telegram: Updates received"
            Logger.debug iLogger "Telegram: Parsing updates..."
            let updates = eitherDecode body :: Either String Updates
            case updates of
                Left err -> fail err
                Right results -> do
                    let events = mapMaybe updateToEvent (uResult results)
                    Logger.debug iLogger "Telegram: Setting update offset..."
                    let newOffset =
                            maximum
                            $ offset
                            : map ((+ 1) . uUpdateId) (uResult results)
                    Logger.debug iLogger
                        $ "Telegram: New update offset: "
                        <> pack (show newOffset)
                    writeIORef iOffset newOffset
                    Logger.debug iLogger
                        $ "Telegram: Current events: " <> pack (show events)
                    return events
        code -> fail $ "Telegram: Request failed: " ++ show code

--------------------------------------------------------------------------------
tgProcessEvents :: IHandle -> [Event] -> IO ()
tgProcessEvents handle [] = return ()
tgProcessEvents handle (e:events) = do
    case e of
        EventMessage {} -> processMessage handle e
        EventMedia {} -> processMedia handle e
        EventQuery {} -> processQuery handle e
    tgProcessEvents handle events

--------------------------------------------------------------------------------
processMessage :: IHandle -> Event -> IO ()
processMessage IHandle {..} EventMessage {..} = undefined
processMessage _ _ = return ()

--------------------------------------------------------------------------------
processMedia :: IHandle -> Event -> IO ()
processMedia IHandle {..} EventMedia {..} = undefined
processMedia _ _ = return ()

--------------------------------------------------------------------------------
processQuery :: IHandle -> Event -> IO ()
processQuery IHandle {..} EventQuery {..} = do
    counters <- readIORef iCounters
    Logger.debug iLogger
        $ "Telegram: Current counters: " <> pack (show counters)
    let address =
            "https://api.telegram.org/bot"
            ++ unpack iToken
            ++ "/answerCallbackQuery"
    let json =
            object
                [ "callback_query_id" .= eQueryId
                , "text" .= ("Updated repeat count to: " <> eUserdata)]
    Logger.info iLogger
        $ "Telegram: Setting repeat count: "
        <> pack (show eChatId)
        <> " : "
        <> eUserdata
    (code,_) <- Web.sendJSON address json
    case code of
        200 -> do
            Logger.info iLogger "Telegram: Set repeat count"
            let newCounter = (read (unpack eUserdata) :: Counter)
            Logger.debug iLogger
                $ "Telegram: Set counter for "
                <> pack (show eChatId)
                <> " to "
                <> eUserdata
            let updatedCounters = M.insert eChatId newCounter counters
            writeIORef iCounters updatedCounters
        _ -> Logger.error iLogger
            $ "Telegram: Failed to answer callback_query "
            <> eQueryId
            <> " with code: "
            <> pack (show code)
processQuery _ _ = return ()
