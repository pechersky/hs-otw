module Main where

import           Text.Read (readMaybe)

import           Natas

main :: IO ()
main =
  fmap readMaybe getLine >>= \case
    Nothing -> pure ()
    Just level -> do
      _res <- runChallenge level
      pure ()
