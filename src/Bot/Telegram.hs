{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Bot.Telegram where

import           Bot                            (Event)
import qualified Bot.Telegram.Internals.Updates as Updates
import           Control.Lens                   ((&), (.~), (^.))
import           Data.Aeson                     (eitherDecode)
import           Data.IORef                     (IORef, readIORef)
import           Data.Text                      (Text, pack, unpack)
import qualified Logger
import           Network.Wreq                   (defaults, getWith, param,
                                                 responseBody, responseStatus,
                                                 statusCode)

-------------------------------------------------------------------------------
data Config =
    Config
    { cToken         :: Text
    , cHelpMessage   :: Text
    , cRepeatMessage :: Text
    , cRepeatCount   :: Int
    , cTimeout       :: Int
    }

-------------------------------------------------------------------------------
data Telegram =
    Telegram
    { tgConfig :: Config
    , tgLogger :: Logger.Handle
    , tgOffset :: IORef Int
    }

-------------------------------------------------------------------------------
tgPoll :: Telegram -> IO [Event]
tgPoll Telegram {..} = do
    oldOffset <- readIORef tgOffset
    Logger.debug tgLogger
        $ "Telegram: Current update offset: " <> pack (show oldOffset)
    Logger.info tgLogger "Telegram: Poll..."
    let options =
            defaults
            & param "offset" .~ [pack $ show oldOffset]
            & param "timeout" .~ [pack $ show (cTimeout tgConfig)]
    response <- getWith options
        $ "https://api.telegram.org/bot"
        ++ unpack (cToken tgConfig)
        ++ "/getUpdates"
    case response ^. responseStatus . statusCode of
        200 -> do
            Logger.info tgLogger "Telegram: Updates received"
            Logger.debug tgLogger "Telegram: Parsing updates..."
            let updates =
                    eitherDecode (response ^. responseBody)
                        :: Either String Updates.Updates
            either
                fail
                (return . map Updates.updateToEvent . Updates.uResult)
                updates
        code -> fail $ "Telegram: Request failed: " ++ show code

-------------------------------------------------------------------------------
tgSendMessage = undefined

-------------------------------------------------------------------------------
tgSendMedia = undefined

-------------------------------------------------------------------------------
tgAnswerQuery = undefined
