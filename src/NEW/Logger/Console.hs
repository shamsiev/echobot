{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module NEW.Logger.Console (newHandle,parseConfig) where

import           Control.Monad (when)
import           Data.Aeson    (FromJSON (parseJSON), eitherDecodeFileStrict,
                                withObject, (.!=), (.:?))
import           NEW.Logger    (Handle (..), Severity (..))
import           Prelude       hiding (error)

newtype Config =
    Config
    { cSeverity :: Severity
    }

instance FromJSON Config where
    parseJSON = withObject "FromJSON Logger.Console.Config" $ \o
        -> Config <$> o .:? "severity" .!= Debug

newHandle :: Config -> IO Handle
newHandle Config {..} =
    return
        Handle
        { debug = \message -> when (Debug >= cSeverity)
              $ putStrLn (show Debug ++ message)
        , info = \message -> when (Info >= cSeverity)
              $ putStrLn (show Info ++ message)
        , warning = \message -> when (Warning >= cSeverity)
              $ putStrLn (show Warning ++ message)
        , error = \message -> when (Error >= cSeverity)
              $ putStrLn (show Error ++ message)
        }

parseConfig :: FilePath -> IO Config
parseConfig filePath = eitherDecodeFileStrict filePath >>= either fail return
