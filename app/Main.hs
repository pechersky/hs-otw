module Main where

import           Text.Read (readMaybe)

import           Natas

main :: IO ()
main = do
  input <- getLine
  runChallenge $ readMaybe input
