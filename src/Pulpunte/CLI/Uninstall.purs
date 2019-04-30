module Pulpunte.CLI.Uninstall
  ( UninstallArgs
  , uninstall
  ) where

import Prelude

import Data.Array (elem, filterA, length, notElem, null, (\\))
import Data.Either (either)
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Unicode ((×))
import Effect.Aff (Aff)
import Effect.Aff.Util (throwError')
import Foreign.Object (empty, filterKeys, keys)
import Node.FS.Aff (exists)
import Node.FS.Extra (remove)
import Node.Path (concat)
import Prelude.Unicode ((∘))
import Pulpunte.Config (Config, readConfig, writeConfig)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Package (PackageName)
import Pulpunte.PackageSet (getPackageSet, packageSetDir, resolveDependencies)

type UninstallArgs =
  { packages ∷ Array PackageName
  }

uninstall ∷ Console → UninstallArgs → Aff Unit
uninstall console { packages: specifiedPackages } = do
  when (null specifiedPackages) $
    throwError' $ msg.uninstall.errNotSpecified

  config ← readConfig

  let allDependencies = config.dependencies
                     <> maybe [] keys config.additions
                     <> fromMaybe [] config.devDependencies
                     <> maybe [] keys config.devAdditions
      notDeps = specifiedPackages \\ allDependencies

  unless (null notDeps) do
    throwError' $ msg.uninstall.errNotExistInDeps $ console.strong <$> notDeps

  let newConfig = config
        { dependencies = config.dependencies \\ specifiedPackages
        , devDependencies = (_ \\ specifiedPackages) <$> config.devDependencies
        , additions = filterKeys (_ `notElem` specifiedPackages) <$> config.additions
        , devAdditions = filterKeys (_ `notElem` specifiedPackages) <$> config.devAdditions
        }

  removePackageFiles console config newConfig specifiedPackages
  writeConfig newConfig
  console.info $ msg.uninstall.removedFromDeps $ console.em <$> specifiedPackages


removePackageFiles ∷ Console → Config → Config → Array PackageName → Aff Unit
removePackageFiles console config newConfig specifiedPackages = do
  packages ← getPackageSet config.packageSet

  let oldAllPackages = fromMaybe empty (config.additions <> config.devAdditions) <> packages
      newDependencies = Just newConfig.dependencies <> newConfig.devDependencies
      newAdditions = newConfig.additions <> newConfig.devAdditions
      newAllPackages = fromMaybe empty newAdditions <> packages
      newAllDependencies = fromMaybe [] $ newDependencies <> (keys <$> newAdditions)

  removable ← either dependencyError pure do
    removePackages ← resolveDependencies oldAllPackages specifiedPackages
    newDepPackages ← resolveDependencies newAllPackages newAllDependencies
    pure $ removePackages `((\\) `on` map fst)` newDepPackages

  notNeededPackages ← filterA (exists ∘ snd) $ addPath <$> removable

  unless (null notNeededPackages) do
    for_ notNeededPackages \package → do
      console.log $ msg.uninstall.uninstalling $ fst package
      remove $ snd package

    console.newline
    console.info $ msg.uninstall.done $ length notNeededPackages

  where
    dependencyError =
      ifM (_ `elem` specifiedPackages)
        (throwError' ∘ msg.uninstall.errStillNeeded ∘ console.strong)
        ((specifiedPackages <$ _) ∘ console.warn
                     ∘ msg.uninstall.brokenDependencies ∘ console.strong)

    addPath = \packageName →
      packageName × concat [packageSetDir config.packageSet, packageName]
