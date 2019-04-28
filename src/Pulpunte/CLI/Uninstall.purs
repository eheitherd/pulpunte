module Pulpunte.CLI.Uninstall
  ( UninstallArgs
  , uninstall
  ) where

import Prelude

import Data.Array (elem, filterA, length, notElem, null, (\\))
import Data.Either (either)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Unicode ((×))
import Effect.Aff (Aff)
import Effect.Aff.Util (throwError')
import Foreign.Object (Object, empty, filterKeys, keys, union)
import Node.FS.Aff (exists)
import Node.FS.Extra (remove)
import Node.Path (concat)
import Prelude.Unicode ((∘))
import Pulpunte.Config (Config, readConfig, writeConfig)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Package (PackageName)
import Pulpunte.PackageSet (PackageInfo, getPackageSet, packageSetDir, resolveDependencies)

type UninstallArgs =
  { packages ∷ Array PackageName
  }

uninstall ∷ Console → UninstallArgs → Aff Unit
uninstall console { packages: specifiedPackages } = do
  when (null specifiedPackages) $
    throwError' $ msg.uninstall.errNotSpecified

  config ← readConfig

  let additions = maybe [] keys config.additions
      allDependencies = config.dependencies <> additions
      notDeps = specifiedPackages \\ allDependencies

  unless (null notDeps) do
    throwError' $ msg.uninstall.errNotExistInDeps $ console.strong <$> notDeps

  let uninstallInfo =
        { specifiedPackages
        , newDependencies: config.dependencies \\ specifiedPackages
        , newAdditions: filterKeys (_ `notElem` specifiedPackages) <$> config.additions
        }

  removePackageFiles console config uninstallInfo
  removeDependencies console config uninstallInfo


type UninstallInfo =
  { specifiedPackages ∷ Array PackageName
  , newDependencies ∷ Array PackageName
  , newAdditions ∷ Maybe (Object PackageInfo)
  }

removeDependencies ∷ Console → Config → UninstallInfo → Aff Unit
removeDependencies console config info = do
  writeConfig $ config { dependencies = info.newDependencies
                       , additions = info.newAdditions
                       }
  console.info $ msg.uninstall.removedFromDeps
               $ console.em <$> info.specifiedPackages


removePackageFiles ∷ Console → Config → UninstallInfo → Aff Unit
removePackageFiles console config info = do
  packages ← getPackageSet config.packageSet

  let allPackages = fromMaybe empty config.additions `union` packages
      newAllPackages = fromMaybe empty info.newAdditions `union` packages
      newAllDependencies = info.newDependencies <> maybe [] keys info.newAdditions

  removable ← either dependencyError pure do
    removePackages ← resolveDependencies allPackages info.specifiedPackages
    newDepPackages ← resolveDependencies newAllPackages newAllDependencies
    pure $ removePackages `((\\) `on` map fst)` newDepPackages

  needPackages ← filterA (exists ∘ snd) $ addPath <$> removable

  unless (null needPackages) do
    for_ needPackages \package → do
      console.log $ msg.uninstall.uninstalling $ fst package
      remove $ snd package

    console.newline
    console.info $ msg.uninstall.done $ length needPackages

  where
    dependencyError =
      ifM (_ `elem` info.specifiedPackages)
        (throwError' ∘ msg.uninstall.errStillNeeded ∘ console.strong)
        ((info.specifiedPackages <$ _) ∘ console.warn
                     ∘ msg.uninstall.brokenDependencies ∘ console.strong)

    addPath = \packageName →
      packageName × concat [packageSetDir config.packageSet, packageName]
