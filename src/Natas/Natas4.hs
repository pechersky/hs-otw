module Natas.Natas4 where

import           Control.Lens
import           Data.String               (fromString)
import           Network.HTTP.Types.Header (hReferer)
import           Network.Wreq              (header)
import           Safe                      (lastMay)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let referer = header hReferer .~ [fromString (parentUri 5 ++ "/")]
  req <- accessLevel' 4 (parentUri 4) referer
  let body = reqBody req
      match = workupBody 5 body
  pure $ match >>= lastMay
