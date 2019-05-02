module Pulpunte.PackageSet
  ( PackageSet
  , PackageInfo
  , Packages
  , packageDir
  , getPackageSet
  , getPackageInfo
  , resolveDependencies
  , PackageTree
  , dependencyTree
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Comonad.Cofree (Cofree, (:<))
import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Data.Argonaut (decodeJson)
import Data.Array (any, snoc, uncons)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Unicode (type (×), (×))
import Effect.Aff (Aff, catchError, message)
import Effect.Aff.Util (eToAff, throwError')
import Foreign.Object (Object, lookup)
import Node.FS.Aff (exists)
import Node.FS.Extra (readJson)
import Node.Git (URL, cloneShallow)
import Node.Path (FilePath, concat)
import Prelude.Unicode ((∘))
import Pulpunte.Cache (cacheRootDir, cleanCache, withTemporary)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Package (PackageName, Place, SpecifiedVersion(..), Version, getDependencies, getLatestVersion)


type PackageSet =
  { repo ∷ URL
  , set ∷ Version
  }

type PackageInfo =
  { dependencies ∷ Array PackageName
  , repo ∷ URL
  , version ∷ Version
  }

type Packages = Object PackageInfo


setDir ∷ FilePath
setDir = ".set"

jsonFile ∷ FilePath
jsonFile = "packages.json"


packageDir ∷ PackageSet → PackageName → FilePath
packageDir packageSet packageName
  = concat [cacheRootDir, packageSet.set, packageName]


getPackageSet ∷ PackageSet → Aff Packages
getPackageSet packageSet = do
  let
    setPath = packageDir packageSet setDir
    jsonPath = concat [setPath, jsonFile]

  unlessM (exists jsonPath) do
    cleanCache
    cloneShallow setPath packageSet.repo packageSet.set

  readPackageSet jsonPath

  where
    readPackageSet = eToAff ∘ decodeJson <=< readJson


getPackageInfo
   ∷ Console
  → Packages
  → String × Maybe Place
  → Aff (Maybe PackageInfo)
getPackageInfo _ _ (_ × Nothing) = pure Nothing
getPackageInfo console packages (packageName × Just place) =
  withTemporary \temporaryDir → replaceError $ runMaybeT do
    repo ← pure' $ place.repo <|> _.repo <$> lookup packageName packages

    version ← case place.version of
      SpecifiedVersion version → pure version
      Latest → lift $ getLatestVersion temporaryDir packageName repo

    maybe (pure unit) (inPackageSet repo version) $ lookup packageName packages

    dependencies ← lift $ getDependencies temporaryDir packageName repo version

    pure { dependencies, repo, version }

  where
    pure' ∷ ∀ a. Maybe a → MaybeT Aff a
    pure' = MaybeT ∘ pure

    replaceError = flip catchError \e → do
      console.error $ message e
      throwError' $ msg.packageSet.errGetPackage $ console.strong packageName

    inPackageSet repo version packageInfo =
      if repo == packageInfo.repo && version == packageInfo.version
        then pure' Nothing
        else pure unit


type PackageList = Array PackageName
type DepPackages = Array (PackageName × PackageInfo)

resolveDependencies ∷ Packages → PackageList → Either PackageName DepPackages
resolveDependencies packages packageList =
  tailRec (resolve packages) $ packageList × []


resolve
   ∷ Packages
  → PackageList × DepPackages
  → Step (PackageList × DepPackages) (Either PackageName DepPackages)
resolve packages (packageList × depPackages) =
  case uncons packageList of
    Just {head, tail} →
      if any ((_ == head) ∘ fst) depPackages
        then Loop $ tail × depPackages
        else case lookup head packages of
          Just info → Loop $ (tail <> info.dependencies)
                                × (snoc depPackages $ head × info)
          Nothing → Done $ Left head

    Nothing → Done $ Right depPackages


type PackageTree = Cofree Array (PackageName × PackageInfo)

dependencyTree
   ∷ Int
  → Packages
  → PackageList
  → Either String (Array PackageTree)
dependencyTree depth packages packageList
  | depth <= 0 = Right []
  | otherwise = traverse mkTree packageList

  where
    mkTree packageName
      | Just info ← lookup packageName packages
        = ((packageName × info) :< _)
               <$> dependencyTree (depth - 1) packages info.dependencies
      | otherwise = Left packageName
