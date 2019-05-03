module Pulpunte.CLI.Install
  ( InstallArgs
  , install
  ) where

import Prelude

import Data.Array (catMaybes, filter, group, nubByEq, null, sort, (\\))
import Data.Array.NonEmpty (head, length)
import Data.Array.Unicode ((∩))
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Profunctor.Strong ((&&&))
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Unicode (type (×))
import Effect.Aff (Aff)
import Effect.Aff.Util (throwError')
import Foreign.Object (empty, fromFoldable, keys, toUnfoldable)
import Prelude.Unicode ((∘))
import Pulpunte.CLI.Install.Parser (parsePackage)
import Pulpunte.Config (Config, readConfig, writeConfig)
import Pulpunte.Console (Console)
import Pulpunte.Install (installAll, installPackages)
import Pulpunte.Message (msg)
import Pulpunte.Package (PackageName)
import Pulpunte.PackageSet (PackageInfo, getPackageInfo, getPackageSet)


type InstallArgs =
  { packages ∷ Array String
  , saveDev ∷ Boolean
  , jobs ∷ Int
  }


install ∷ Console → InstallArgs → Aff Unit
install console { packages: packageList, saveDev, jobs } = do
  config ← readConfig

  if null packageList
    then do
      when saveDev $ console.warn msg.install.ignoreSaveDev
      installAll console jobs config
    else
      installNew console jobs config saveDev packageList


installNew ∷ Console → Int → Config → Boolean → Array String → Aff Unit
installNew console jobs config saveDev packageList = do
  let packageNamesWithPlaces = parsePackage <$> packageList
      packageNames = fst <$> packageNamesWithPlaces

  let duplicated = head <$> filter ((_ > 1) ∘ length) (group packageNames)
  unless (null duplicated) $
    throwError' $ msg.install.errDuplicated $ console.strong <$> duplicated

  packages ← getPackageSet config.packageSet
  packageNamesWithInfos
    ← traverse (sequence ∘ (fst &&& getPackageInfo console packages))
                                                    packageNamesWithPlaces

  let additions = fromMaybe empty config.additions
      devAdditions = fromMaybe empty config.devAdditions
      devDependencies = fromMaybe [] config.devDependencies
      allDependencies = keys additions <> config.dependencies
                     <> keys devAdditions <> devDependencies

  let installed = packageNames ∩ allDependencies
  unless (null installed) $
    throwError' $ msg.install.alreadyInstalled $ console.strong <$> installed

  let notInstalled = packageNames \\ allDependencies
      newAdditionList = catMaybes $ sequence <$> packageNamesWithInfos

  let dependableAdditions = fromFoldable newAdditionList
                         <> additions <> guard saveDev devAdditions
  installPackages console jobs config.packageSet dependableAdditions packageNames

  unless (null notInstalled) do
    writeConfig $ updateConfig saveDev newAdditionList notInstalled config
    console.info $ msg.install.addedToDeps $ console.em <$> notInstalled


updateConfig
   ∷ Boolean
  → Array (PackageName × PackageInfo)
  → Array PackageName
  → Config
  → Config
updateConfig saveDev newAdditionList newPackages config =
  let newDependencies = newPackages \\ (fst <$> newAdditionList)
   in if saveDev
        then config
          { devDependencies = updateDevDependencies config.devDependencies newDependencies
          , devAdditions = updateAddition config.devAdditions newAdditionList
          }
        else config
          { dependencies = sort $ config.dependencies <> newDependencies
          , additions = updateAddition config.additions newAdditionList
          }

  where
    updateDevDependencies devDependencies newDependencies =
      if null newDependencies
        then devDependencies
        else Just $ sort $ newDependencies <> fromMaybe [] devDependencies

    updateAddition additions newAdditionList' =
      if null newAdditionList'
        then additions
        else Just $ fromFoldable $ sort $ nubByEq (eq `on` fst)
                  $ newAdditionList' <> toUnfoldable (fromMaybe empty additions)
