module Snake.IO where

import Snake.Core

-- | Reads a grid layout from a file
loadGrid :: FilePath -> IO Grid
loadGrid filename = do
    str <- readFile filename
    case readGrid str of
        Nothing -> fail $ "Invalid grid level: " ++ filename
        Just x  -> return x
