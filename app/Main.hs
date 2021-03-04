{-# LANGUAGE OverloadedStrings #-}

module Main where

import Logger (Level(Debug), debug)
import Logger.StdLogger (new)

main :: IO ()
main = new Debug >>= (`debug` "Testing logger")
