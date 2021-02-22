module Main where

import           Bot
import qualified Bot.Telegram       as Telegram
import qualified Bot.VK             as VK
import qualified Logger.Console     as Logger
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["telegram"] -> do
      hLogger <- Logger.new =<< Logger.parseConfig "telegram.json"
      bot =<< (`Telegram.new` hLogger) =<< Telegram.parseConfig "telegram.json"
    ["vk"] -> do
      hLogger <- Logger.new =<< Logger.parseConfig "vk.json"
      bot =<< (`VK.new` hLogger) =<< VK.parseConfig "vk.json"
    _ -> return ()
