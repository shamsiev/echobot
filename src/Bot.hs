module Bot where

newtype Handle =
  Handle
    { bot :: IO ()
    }
