module Main where

import qualified Lib
import qualified Path
import qualified Path.IO as Path
import System.Environment

main :: IO ()
main = do
    [inputS, outputS] <- getArgs
    input <- Path.resolveDir' inputS
    output <- Path.resolveDir' outputS
    Lib.stripArticles input output

