module NEW.Bot where

newtype Handle =
    Handle
    { start :: FilePath -> IO ()
    }
