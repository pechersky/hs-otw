{-# LANGUAGE OverloadedStrings #-}

module Natas.Inject where

import           Control.Monad       (filterM)

import           Data.List           (intersperse)
import           Data.Text           (pack)
import           Data.Time           (NominalDiffTime, diffUTCTime,
                                      getCurrentTime)

import           Control.Monad.Loops (firstM)

import           Natas.Natas

injectSolution ::
     (Char -> IO Bool) -> (String -> String -> IO (Maybe String)) -> Solution
injectSolution attemptChar build = do
  presentChars <- filterM attemptChar validChars
  print presentChars
  match <- build presentChars ""
  pure $ fmap pack match

buildPassword' :: (String -> IO Bool) -> String -> String -> IO (Maybe String)
buildPassword' attempt avail curr = do
  let placeEnd x = curr ++ pure x
  matches <- (firstM attempt . fmap placeEnd) avail
  case matches of
    Nothing -> pure (pure curr)
    Just match -> do
      print match
      buildPassword' attempt avail match

placeAnywhereSql :: Char -> String
placeAnywhereSql x = intersperse x "%%"

validChars :: String
validChars = ['0' .. '9'] ++ ['A' .. 'Z'] ++ ['a' .. 'z']

timeAction :: IO a -> IO (NominalDiffTime, a)
timeAction action = do
  before <- getCurrentTime
  result <- action
  after <- getCurrentTime
  pure (diffUTCTime after before, result)

timeAction_ :: IO a -> IO NominalDiffTime
timeAction_ = fmap fst . timeAction
