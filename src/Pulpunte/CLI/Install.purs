module Pulpunte.CLI.Install
  ( InstallArgs
  , install
  ) where

import Prelude

import Data.Array (catMaybes, filter, group, nubByEq, null, sort, sortWith, (\\))
import Data.Array.NonEmpty (head, length)
import Data.Array.Unicode ((∩))
import Data.Function (on)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor.Strong ((&&&))
import Data.Traversable (sequence, traverse)
import Data.Tuple (fst)
import Data.Tuple.Unicode (type (×))
import Effect.Aff (Aff)
import Effect.Aff.Util (throwError')
import Foreign.Object (Object, empty, fromFoldable, keys, toUnfoldable, union)
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
  , jobs ∷ Int
  }


install ∷ Console → InstallArgs → Aff Unit
install console { packages: packageList, jobs } = do
  config ← readConfig

  if null packageList
    then installAll console jobs config
    else installNew console jobs config packageList


installNew ∷ Console → Int → Config → Array String → Aff Unit
installNew console jobs config packageList = do
  let packageNamesWithPlaces = parsePackage <$> packageList
      packageNames = fst <$> packageNamesWithPlaces
      duplicated = head <$> filter ((_ > 1) ∘ length) (group packageNames)

  unless (null duplicated) $
    throwError' $ msg.install.errDuplicated $ console.strong <$> duplicated

  packages ← getPackageSet config.packageSet
  packageNamesWithInfos
    ← traverse (sequence ∘ (fst &&& getPackageInfo console packages))
                                                    packageNamesWithPlaces

  let additions = fromMaybe empty config.additions
      dependencies = keys additions <> config.dependencies
      installed = packageNames ∩ dependencies

  unless (null installed) $
    console.warn $ msg.install.alreadyInstalled $ console.strong <$> installed

  let notInstalled = packageNames \\ dependencies
      newAdditionList = catMaybes $ sequence <$> packageNamesWithInfos
      newAdditions = fromFoldable newAdditionList `union` additions
      newDependencies = notInstalled \\ (fst <$> newAdditionList)

  installPackages console jobs config.packageSet newAdditions packageNames

  unless (null notInstalled) do
    writeConfig $
      config
        { dependencies = sort $ config.dependencies <> newDependencies
        , additions = updateAddition config.additions newAdditionList
        }
    console.info $ msg.install.addedToDeps $ console.em <$> notInstalled


updateAddition
   ∷ Maybe (Object PackageInfo)
  → Array (PackageName × PackageInfo)
  → Maybe (Object PackageInfo)
updateAddition additions newAdditionList
  | Just infos ← additions
      = Just $ fromFoldable $ sortWith fst $ nubByEq (eq `on` fst)
                            $ newAdditionList <> toUnfoldable infos
  | otherwise = newAdditionList
      # ifM null (const Nothing) (Just ∘ fromFoldable ∘ sortWith fst)
