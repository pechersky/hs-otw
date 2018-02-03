module Natas.Natas7 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (listToMaybe)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  sreq <- accessLevel 7
  let Just subpath = workupComments ("natas8" `elem`) sreq
      page = param "page" .~ [subpath]
  req <- accessLevel' 7 (parentUri 7) page
  let body = (decodeUtf8 . C.toStrict) $ req ^. responseBody
      match = (T.words . T.strip . innerText . parseTags) body
  pure $ (listToMaybe . reverse) match
