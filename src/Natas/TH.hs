module Natas.TH where

import           Data.Char                  (isDigit)
import           Data.List                  (isInfixOf)
import           Data.Maybe                 (catMaybes)

import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Syntax

getChallenges :: ExpQ
getChallenges = do
  currmod <- thisModule
  ModuleInfo xs <- reifyModule currmod
  let natasModules =
        filter
          (\(Module _ mname) -> isInfixOf "Natas" (modString mname))
          xs
  solns <- traverse moduleToSoln natasModules
  listE (catMaybes solns)

moduleToSoln :: Module -> Q (Maybe ExpQ)
moduleToSoln (Module _ (ModName modname))
  | null moduleNumberStr = pure Nothing
  | otherwise = do
    solnName <- lookupValueName (modname ++ ".solution")
    case solnName of
      Nothing -> pure Nothing
      Just name ->
        pure $ pure (tupE [litE (integerL (read moduleNumberStr)), varE name])
  where
    moduleNumberStr = filter isDigit modname
