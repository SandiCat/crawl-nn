module Main where

import qualified Lib
import qualified Path
import qualified Path.IO as Path
import System.Environment

main :: IO ()
main = do
    [inputS, outputS] <- getArgs
    input <- Path.resolveFile' inputS
    output <- Path.resolveDir' outputS
    Lib.downloadArticles input output

