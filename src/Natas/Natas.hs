{-# LANGUAGE RankNTypes #-}

module Natas.Natas where

import           Control.Exception    (catchJust)

import qualified Data.ByteString      as B
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO         as TIO

import           Network.HTTP.Client  (HttpException (HttpExceptionRequest), HttpExceptionContent (StatusCodeException))
import           Network.Wreq
import           Network.Wreq.Types   (Postable)

import           Control.Lens

type Solution = IO (Maybe Text)

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

getLevel :: Int -> IO (Response ByteString)
getLevel level = getLevel' level (parentUri level) id

getLevel' :: Int -> String -> (Options -> Options) -> IO (Response ByteString)
getLevel' level uri modifyOptions = do
  opts <- modifyOptions <$> loginOptions level
  getWith opts uri

postLevel :: (Postable a) => Int -> a -> IO (Response ByteString)
postLevel level = postLevel' level (parentUri level) id

postLevel' ::
     (Postable a)
  => Int
  -> String
  -> (Options -> Options)
  -> a
  -> IO (Response ByteString)
postLevel' level uri modifyOptions form = do
  opts <- modifyOptions <$> loginOptions level
  postWith opts uri form

loginOptions :: Int -> IO Options
loginOptions level = do
  password <- encodeUtf8 <$> readPassword level
  let username = (encodeUtf8 . T.pack) ("natas" ++ show level)
  pure $ defaults & auth ?~ basicAuth username password

readPassword :: Int -> IO Text
readPassword n = T.strip <$> TIO.readFile ("passwords/natas" ++ show n)

writePassword :: Int -> Text -> IO ()
writePassword n = TIO.writeFile ("passwords/natas" ++ show n) . T.strip

reqBody :: Response ByteString -> Text
reqBody = decodeUtf8 . toStrict . view responseBody

isStatusCodeExceptionHandler ::
     (forall body. Response body -> B.ByteString -> a)
  -> HttpException
  -> Maybe a
isStatusCodeExceptionHandler converter (HttpExceptionRequest _req (StatusCodeException resp respbody)) =
  Just (converter resp respbody)
isStatusCodeExceptionHandler _converter _exception = Nothing

catchResponseHandler ::
     (forall body. Response body -> a) -> IO a -> IO a
catchResponseHandler converter req =
  catchJust (isStatusCodeExceptionHandler (const . converter)) req pure

catchResponseBodyHandler :: (B.ByteString -> a) -> IO a -> IO a
catchResponseBodyHandler converter req =
  catchJust (isStatusCodeExceptionHandler (const converter)) req pure
