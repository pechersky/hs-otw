{-# LANGUAGE OverloadedStrings #-}

module Natas.Natas11 where

import           Data.Bits              (xor)
import           Data.List              (inits)

import qualified Data.ByteString        as B
import           Data.ByteString.Base64 (decodeLenient, encode)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           Safe                   (headMay, lastMay)

import           Data.Aeson             (Value)
import qualified Data.Aeson             as A
import           Network.Wreq           (cookie, cookieValue, cookies, postWith,
                                         responseCookie, responseCookieJar)

import           Control.Lens
import           Data.Aeson.Lens

import           Natas.Natas
import           Natas.Parse

solution :: Solution
solution = do
  opts <- loginOptions 11
  req <- accessLevel 11
  let ckis = req ^.. responseCookie "data"
      Just secret = ckis ^? folded . cookieValue
      encKeyL =
        transcode (A.encode payload) (LB.fromStrict (decodeLenient secret))
      Just encKey = generateKey encKeyL
      newpayload = payload & key "showpassword" .~ "yes"
      encSecret = encodeSecret encKey (A.encode newpayload)
      modifyCookie = (cookie "data" . cookieValue) .~ encSecret
      placeJar = cookies ?~ (req ^. responseCookieJar)
  ckireq <- postWith ((modifyCookie . placeJar) opts) (parentUri 11) newpayload
  let body = reqBody ckireq
      match = workupBody 12 body
  pure $ match >>= lastMay

getSecret :: IO (Maybe B.ByteString)
getSecret = do
  req <- accessLevel 11
  let secret = req ^.. responseCookie "data" . cookieValue
  pure $ headMay secret

payload :: Value
payload =
  A.object ["showpassword" A..= T.pack "no", "bgcolor" A..= T.pack "#ffffff"]

transcode :: ByteString -> ByteString -> ByteString
transcode x y = LB.pack (LB.zipWith xor x y)

encodeSecret :: ByteString -> ByteString -> B.ByteString
encodeSecret keyBS = encode . LB.toStrict . transcode keyBS

extractKey :: B.ByteString -> Maybe B.ByteString
extractKey = fmap (encodeUtf8 . T.pack) . findRepeat . T.unpack . decodeUtf8

generateKey :: ByteString -> Maybe ByteString
generateKey = fmap (LB.cycle . LB.fromStrict) . extractKey . LB.toStrict

findRepeat :: Eq a => [a] -> Maybe [a]
findRepeat xs = (headMay . filter go . tail . inits) xs
  where
    go ys = and (zipWith (==) xs (cycle ys))
