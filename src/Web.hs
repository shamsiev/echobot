module Web where

import Control.Lens ((^.))
import Data.Aeson (Value, object)
import Data.ByteString.Lazy.Internal (ByteString)
import Network.Wreq
  ( Options
  , defaults
  , getWith
  , post
  , postWith
  , responseBody
  , responseStatus
  , statusCode
  )

--------------------------------------------------------------------------------
type Address = String

type JSON = Value

type StatusCode = Int

type RequestBody = ByteString

--------------------------------------------------------------------------------
sendJSON :: Address -> JSON -> IO (StatusCode, RequestBody)
sendJSON address json = do
  response <- post address json
  let code = response ^. responseStatus . statusCode
  let body = response ^. responseBody
  return (code, body)

--------------------------------------------------------------------------------
sendOptions :: Address -> Options -> IO (StatusCode, RequestBody)
sendOptions address options = do
  response <- getWith options address
  let code = response ^. responseStatus . statusCode
  let body = response ^. responseBody
  return (code, body)
