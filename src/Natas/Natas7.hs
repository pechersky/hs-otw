module Natas.Natas7 where

import           Control.Lens
import qualified Data.Text         as T
import           Network.Wreq      (param)
import           Safe              (lastMay)
import           Text.HTML.TagSoup (innerText, parseTags)

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  sreq <- accessLevel 7
  let Just subpath = workupComments 8 sreq
      page = param "page" .~ [subpath]
  req <- accessLevel' 7 (parentUri 7) page
  let body = reqBody req
      match = (T.words . T.strip . innerText . parseTags) body
  pure $ lastMay match
