module Logger (Handle(..),Severity(..),debug,info,warning,error) where

import           Data.Text (Text)
import           Prelude   hiding (error, log)

-------------------------------------------------------------------------------
newtype Handle =
    Handle
    { log :: Severity -> Text -> IO ()
    }

-------------------------------------------------------------------------------
data Severity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Ord,Eq)

instance Show Severity where
    show Debug   = "[ DEBUG ] "
    show Info    = "[ INFO  ] "
    show Warning = "[ WARN  ] "
    show Error   = "[ ERROR ] "

-------------------------------------------------------------------------------
debug :: Handle -> Text -> IO ()
debug = (`log` Debug)

info :: Handle -> Text -> IO ()
info = (`log` Info)

warning :: Handle -> Text -> IO ()
warning = (`log` Warning)

error :: Handle -> Text -> IO ()
error = (`log` Error)
