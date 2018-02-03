module Natas.Natas5 where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8   as C
import           Data.Maybe                   (catMaybes, listToMaybe)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8)
import           Network.HTTP.Client.Internal (createCookieJar, expose)
import           Network.Wreq
import           Text.HTML.TagSoup

import           Natas.Natas

solution :: Solution
solution = do
  opts <- loginOptions 5
  req <- getWith opts (parentUri 5)
  let Just cki = (listToMaybe . expose . view responseCookieJar) req
      ckiopts =
        opts &
        set cookies (pure (createCookieJar (pure (cki & cookieValue .~ "1"))))
  ckireq <- getWith ckiopts (parentUri 5)
  let body = (decodeUtf8 . C.toStrict) $ ckireq ^. responseBody
      match =
        (filter ("natas6" `elem`) .
         fmap T.words . catMaybes . fmap maybeTagText . parseTags)
          body
  pure $ listToMaybe match >>= (listToMaybe . reverse)
