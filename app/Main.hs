module Main where

import           Control.Monad.Trans.Maybe (runMaybeT)
import           Text.Read                 (readMaybe)

import           Natas

main :: IO ()
main = do
  input <- getLine
  _ <- runMaybeT $ runChallenge (readMaybe input)
  pure ()
