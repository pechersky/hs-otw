module Natas.Natas where

import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO         as TIO

import           Network.Wreq
import           Network.Wreq.Types   (Postable)

import           Control.Lens

type Solution = IO (Maybe Text)

parentUri :: Int -> String
parentUri level = "http://natas" ++ show level ++ ".natas.labs.overthewire.org"

getLevel :: Int -> IO (Response ByteString)
getLevel level = getLevel' level (parentUri level) id

getLevel' ::
     Int -> String -> (Options -> Options) -> IO (Response ByteString)
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
