module Main where

import           Text.Read (readMaybe)

import           Natas

main :: IO ()
main = do
  fmap readMaybe getLine >>= \case
    Nothing -> pure ()
    Just level -> do
      _ <- runChallenge level
      pure ()
