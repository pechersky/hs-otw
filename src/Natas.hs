{-# LANGUAGE TemplateHaskell #-}

module Natas
  ( challenges
  , runChallenge
  ) where

import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.Map                  as M

import           Natas.Natas
import           Natas.TH

import           Natas.Natas0              (solution)
import           Natas.Natas1              (solution)
import           Natas.Natas2              (solution)
import           Natas.Natas3              (solution)
import           Natas.Natas4              (solution)
import           Natas.Natas5              (solution)
import           Natas.Natas6              (solution)
import           Natas.Natas7              (solution)
import           Natas.Natas8              (solution)
import           Natas.Natas9              (solution)
import           Natas.Natas10             (solution)
import           Natas.Natas11             (solution)
import           Natas.Natas12             (solution)
import           Natas.Natas13             (solution)
import           Natas.Natas14             (solution)
import           Natas.Natas15             (solution)
import           Natas.Natas16             (solution)
import           Natas.Natas17             (solution)

challenges :: M.Map Int Solution
challenges = M.fromList $(getChallenges)

runChallenge :: Int -> IO (Maybe ())
runChallenge n =
  runMaybeT $ do
    Just act <- pure $ M.lookup n challenges
    newPassword <- MaybeT act
    lift $ print newPassword
    lift $ writePassword (n + 1) newPassword
