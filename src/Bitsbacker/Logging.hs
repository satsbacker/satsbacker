
module Bitsbacker.Logging
    ( logError
    ) where

import System.IO (stderr, hPutStrLn)

logError :: String -> IO ()
logError err = hPutStrLn stderr err



