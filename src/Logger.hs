module Logger (Logger(..),Severity(..),debug,info,warning,error) where

import           Data.Text (Text)
import           Prelude   hiding (error, log)

class Monad m => Logger m where
    log :: Severity -> Text -> m ()

data Severity
    = Debug
    | Info
    | Warning
    | Error
    deriving (Eq,Ord)

instance Show Severity where
    show Debug   = "[ DEBUG ] "
    show Info    = "[ INFO  ] "
    show Warning = "[ WARN  ] "
    show Error   = "[ ERROR ] "

debug :: Logger m => Text -> m ()
debug = log Debug

info :: Logger m => Text -> m ()
info = log Info

warning :: Logger m => Text -> m ()
warning = log Warning

error :: Logger m => Text -> m ()
error = log Error
