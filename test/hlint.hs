module Main where

import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure, exitSuccess)

args :: [String]
args = ["bin", "src"]

main :: IO ()
main = do
  hints <- (hlint args)
  if null hints then exitSuccess
                else exitFailure
