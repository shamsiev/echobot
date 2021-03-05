{-# LANGUAGE OverloadedStrings #-}

module Main where

import Logger
import Logger.Console

main :: IO ()
main = do
  h <- new Debug
  debug h "debug message :)"
  return ()
