module Logger.Test where

import Logger (Handle(..), Level, Logger(Logger, NoLogger))
import Prelude hiding (log)

new :: Level -> IO (Handle Logger)
new minLevel =
  return
    Handle
      { log =
          \level message ->
            if level >= minLevel
              then return $ Logger "time" level message
              else return NoLogger
      }
