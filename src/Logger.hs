{-# LANGUAGE OverloadedStrings #-}

module Logger
  (Handle(..)
  ,Level(..)
  ,loggerFilter
  ,withNullLogger
  ,withFileLogger
  ,withTestLogger
  ,withStrLogger
  ,withMultiLogger
  ,debug
  ,info
  ,warning
  ,error) where

import           Control.Exception (bracket, onException)
import           Control.Monad     (Monad (return, (>>)), MonadFail (fail),
                                    join, when, (=<<))
import           Data.IORef        (modifyIORef', newIORef, readIORef)
import           Data.Text         (Text, pack, unpack)
import qualified Data.Text.IO      as TextIO
import           Data.Time.Clock   (getCurrentTime)
import           Data.Time.Format  (defaultTimeLocale, formatTime)
import           Data.Yaml         (FromJSON (parseJSON), withText)
import           Prelude           hiding (error, log)
import qualified System.IO         as IO

newtype Handle =
    Handle
    { log :: Level -> Text -> IO ()
    }

data Level
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq,Ord)

instance Show Level where
    show Debug   = "[ DEBUG ] "
    show Info    = "[ INFO  ] "
    show Warning = "[ WARN  ] "
    show Error   = "[ ERROR ] "

debug :: Handle -> Text -> IO ()
debug = (`log` Debug)

info :: Handle -> Text -> IO ()
info = (`log` Info)

warning :: Handle -> Text -> IO ()
warning = (`log` Warning)

error :: Handle -> Text -> IO ()
error = (`log` Error)

instance FromJSON Level where
    parseJSON = withText "FromJSON Logger.Level" $ \t -> case t of
        "debug"   -> pure Debug
        "info"    -> pure Info
        "warning" -> pure Warning
        "error"   -> pure Error
        _         -> fail $ "Unknown logger level: " ++ unpack t

loggerFilter :: Level -> Handle -> Handle
loggerFilter minLevel inner = do
    Handle
        { log = \level message -> when (level >= minLevel)
              $ log inner level message
        }

withNullLogger :: (Handle -> IO r) -> IO r
withNullLogger f =
    f
    $ Handle
    { log = \_ _ -> return ()
    }

withFileLogger :: FilePath -> (Handle -> IO r) -> IO r
withFileLogger path f = bracket (newFileLogger path) fst (f . snd)

newFileLogger :: FilePath -> IO (IO (), Handle)
newFileLogger path = do
    fh <- IO.openFile path IO.AppendMode
    IO.hSetEncoding fh =<< IO.mkTextEncoding "UTF-8//TRANSLIT"
    return
        $ (,) (IO.hClose fh)
        $ Handle
        { log = \level message -> do
              time <- getCurrentTime
              let timestr = formatTime defaultTimeLocale "%F %T.%q" time
              TextIO.hPutStrLn fh
                  $ pack (timestr ++ ": " ++ show level) <> message
              IO.hFlush fh
        }

withTestLogger :: (Handle -> IO r) -> IO r
withTestLogger f = do
    pbuf <- newIORef $ return ()
    onException (doBody pbuf) (writeOutput pbuf)
  where
    doBody pbuf = do
        f
            $ Handle
            { log = \level message
                  -> modifyIORef' pbuf (>> sendStd level message)
            }

    writeOutput pbuf = join $ readIORef pbuf

withStrLogger :: (Handle -> IO r) -> IO r
withStrLogger f =
    f
    $ Handle
    { log = sendStd
    }

sendStd :: Level -> Text -> IO ()
sendStd level message = TextIO.putStrLn $ pack (show level) <> message

withMultiLogger :: Handle -> Handle -> (Handle -> IO r) -> IO r
withMultiLogger a b f =
    f
        Handle
        { log = \level message -> do
              log a level message
              log b level message
        }
