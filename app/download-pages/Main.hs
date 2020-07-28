module Main where

import qualified Lib
import qualified Path
import qualified Path.IO as Path
import System.Environment

main :: IO ()
main = do
    [dirS, fromS, toS] <- getArgs
    let Just from = readMaybe fromS
    let Just to = readMaybe toS
    dir <- Path.resolveDir' dirS
    Lib.downloadAllSearches dir $ map Lib.Year [from..to]

