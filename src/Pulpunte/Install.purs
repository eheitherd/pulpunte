module Pulpunte.Install
  ( installAll
  , installPackages
  ) where

import Prelude

import Data.Array (elem, filterA, length, nub)
import Data.Either (either)
import Data.Maybe (fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Unicode ((×))
import Effect.Aff (Aff, catchError, throwError)
import Effect.Aff.Parallel (parallelize_)
import Effect.Aff.Util (throwError')
import Foreign.Object (empty, keys, union)
import Node.FS.Aff (exists)
import Node.FS.Extra (remove)
import Node.Git (cloneShallow)
import Node.Path (concat)
import Prelude.Unicode ((∘))
import Pulpunte.Config (Config)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Package (PackageName)
import Pulpunte.PackageSet (PackageSet, Packages, getPackageSet, packageSetDir, resolveDependencies)


installAll ∷ Console → Int → Config → Aff Unit
installAll console jobs config = do
  let additions = fromMaybe empty (config.additions <> config.devAdditions)
      packageList = nub $ keys additions <> config.dependencies
                                         <> fromMaybe [] config.devDependencies

  installPackages console jobs config.packageSet additions packageList


installPackages
   ∷ Console
  → Int
  → PackageSet
  → Packages
  → Array PackageName
  → Aff Unit
installPackages console jobs packageSet additions packageList = do
  let jobs' = clamp 1 64 jobs

  packages ← union additions <$> getPackageSet packageSet

  depPackages ← either errNotExistInPackaeSet pure $
                  resolveDependencies packages packageList

  needPackages ← filterUninstalled $ addPath <$> depPackages

  let num = length needPackages
  when (num > 0) do
    console.info $ msg.install.currentPackageSet packageSet.repo packageSet.set
    parallelize_ jobs' $ installPackage packageSet <$> needPackages
    console.newline
    console.info $ msg.install.done num

  where
    addPath package = concat [packageSetDir packageSet, fst package] × package

    filterUninstalled = filterA $ map not ∘ exists ∘ fst

    installPackage setRepo (path × (package × info)) = do
      console.log $ msg.install.installing package
      catchError (cloneShallow path info.repo info.version)
        \e → do
          -- If an error occurs, isomorphicGit may leave the directory undeleted.
          remove path
          throwError e

    errNotExistInPackaeSet packageName =
      let message = if packageName `elem` packageList
            then msg.install.errNotExistInPackaeSet
            else msg.install.errDepNotExistInPackageSet
       in throwError' $ message $ console.strong packageName
