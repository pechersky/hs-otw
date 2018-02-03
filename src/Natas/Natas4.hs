module Natas.Natas4 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe                 (listToMaybe, catMaybes)
import           Data.String                (fromString)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8)
import           Network.HTTP.Types.Header  (hReferer)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

solution :: Solution
solution = do
  let referer = header hReferer .~ [fromString (parentUri 5 ++ "/")]
  req <- accessLevel' 4 (parentUri 4) referer
  let body = (decodeUtf8 . C.toStrict) $ req ^. responseBody
      match =
        (filter ("natas5" `elem`) . fmap T.words . catMaybes . fmap maybeTagText . parseTags)
          body
  pure $ listToMaybe match >>= (listToMaybe . reverse)
