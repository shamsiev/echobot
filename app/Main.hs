{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Logger
import qualified Logger.StdLogger as StdLogger

main :: IO ()
main = do
  let config = StdLogger.Config Logger.Debug
  liftIO $ StdLogger.runStdLogger (Logger.debug "Log message") config
