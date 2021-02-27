{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bot.Telegram where

import Bot (Event)
import Bot.Telegram.Internal (Updates(uResult),updateToEvent)
import Data.Aeson (KeyValue((.=)),eitherDecode,object)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.IORef (IORef,readIORef)
import Data.Maybe (mapMaybe)
import Data.Text (Text,pack,unpack)
import qualified Logger
import qualified Web

--------------------------------------------------------------------------------
type Token = Text

type Timeout = Int

type Offset = Int

--------------------------------------------------------------------------------
data IHandle =
    IHandle
    { iToken :: Token
    , iTimeout :: Timeout
    , iOffset :: IORef Offset
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
            Logger.info iLogger "Telegram: Parsing updates..."
            let updates = eitherDecode body :: Either String Updates
            case updates of
                Left err -> fail err
                Right results -> do
                    let events = mapMaybe updateToEvent (uResult results)
                    Logger.debug iLogger
                        $ "Telegram: Current events: " <> pack (show events)
                    return events
        code -> fail $ "Telegram: Request failed: " ++ show code

--------------------------------------------------------------------------------
tgProcessEvents :: IHandle -> [Event] -> IO ()
tgProcessEvents IHandle {..} events = undefined
