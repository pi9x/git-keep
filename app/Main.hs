module Main where

import App (runCommand)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  runCommand args
