{-# LANGUAGE OverloadedStrings #-}

module Logger.Console
  ( Config(..)
  , new
  , parseConfig
  ) where

import           Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict,
                             withObject, (.:?))
import           Data.Maybe (fromMaybe)
import           Logger     (Handle (Handle), Severity (Debug))

newtype Config =
  Config
    { cSeverity :: Maybe Severity
    }

new :: Config -> IO Handle
new (Config severity) = return $ Handle (logConsole $ fromMaybe Debug severity)

logConsole :: Severity -> Severity -> String -> IO ()
logConsole severity s x
  | s >= severity = putStrLn $ show s ++ " " ++ x
  | otherwise = return ()

instance FromJSON Config where
  parseJSON =
    withObject "FromJSON Logger.Console.Config" $ \o ->
      Config <$> o .:? "severity"

parseConfig :: FilePath -> IO Config
parseConfig path = do
  config <- eitherDecodeFileStrict path :: IO (Either String Config)
  either fail return config
