module Pulpunte.CLI.Install.Parser
  ( parsePackage
  ) where

import Prelude

import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Either (hush)
import Data.Foldable (findMap)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Tuple.Unicode (type (×), (×))
import Pulpunte.Package (PackageName, Place, SpecifiedVersion(..))


parsePackage ∷ String → PackageName × Maybe Place
parsePackage str =
  fromMaybe (str × Nothing) $
    findMap (_ $ str)
      [ packageNameWithVersion
      , packageNameWithAccount
      , packageNameWithUrl
      ]


packageNameWithVersion ∷ String → Maybe (String × Maybe Place)
packageNameWithVersion str = do
  list ← match' reg str
  packageName ← join (list !! 1)
  version ← specifiedVersion <$> join (list !! 2)
  pure $ packageName × Just { repo: Nothing, version }

  where
    reg = """^(?:purescript-)?([\w-]+)@(latest|v\d+\.\d+\.\d+)$"""


packageNameWithAccount ∷ String → Maybe (String × Maybe Place)
packageNameWithAccount str = do
  list ← match' reg str
  account ← join (list !! 1)
  packageName ← join (list !! 2)
  let version = maybe Latest specifiedVersion $ join (list !! 3)
  pure $ packageName × Just { repo: Just $ url account packageName, version }

  where
    reg = """^([\w-]+)\/(?:purescript-)?([\w-]+)(?:@(latest|v\d+\.\d+\.\d+))?$"""
    url acc pn = "https://github.com/" <> acc <> "/purescript-" <> pn <> ".git"


packageNameWithUrl ∷ String → Maybe (String × Maybe Place)
packageNameWithUrl str = do
  list ← match' reg str
  repo ← list !! 1
  packageName ← join (list !! 2)
  let version = maybe Latest specifiedVersion $ join (list !! 3)
  pure $ packageName × Just { repo, version }

  where
    reg = """^([a-z]+:\/\/[\w\.\/-~]+\/purescript-([\w-]+)\.git)(?:@(latest|v\d+\.\d+\.\d+))?$"""


match' ∷ String → String → Maybe (NonEmptyArray (Maybe String))
match' regStr str = hush (regex regStr noFlags) >>= flip match str

specifiedVersion ∷ String → SpecifiedVersion
specifiedVersion "latest" = Latest
specifiedVersion version = SpecifiedVersion version
