module Pulpunte.CLI.List
  ( list
  ) where

import Prelude

import Control.Comonad.Cofree (Cofree, mkCofree)
import Control.Comonad.Cofree as C
import Data.Array (elem, intercalate, last, null, sort, uncons, unsnoc)
import Data.Bifunctor (lmap)
import Data.Foldable (foldMap, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Traversable (traverse)
import Data.Tuple.Unicode (type (×), (×))
import Effect.Aff (Aff, catchError)
import Effect.Aff.Util (eToAff)
import Foreign.Object (empty, keys, member)
import Node.Git (listTags)
import Prelude.Unicode ((∘), (≠))
import Pulpunte.Config (Config, readConfig)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Package (Version, PackageName)
import Pulpunte.PackageSet (PackageInfo, dependencyTree, getPackageSet, packageDir, resolveDependencies)


type ListArgs =
  { depth ∷ Int
  , flat ∷ Boolean
  }

maxDepth ∷ Int
maxDepth = 20

list ∷ Console → ListArgs → Aff Unit
list console { depth, flat } = do
  config ← readConfig
  packages ← getPackageSet config.packageSet

  let allAdditions = fromMaybe empty $ config.additions <> config.devAdditions
      allDependencies = config.dependencies
                     <> fromMaybe [] config.devDependencies
                     <> keys allAdditions
      allPackages = allAdditions <> packages
      initialDepth = clamp 1 maxDepth depth

  infos ← if flat then getFlat allPackages allDependencies
                   else getTree initialDepth allPackages allDependencies
  statuses ← traverse (traverse $ packageStatus config) infos
  printTree console [] statuses

  where
    getTree initialDepth packages packageList =
      eToAff $ lmap errDep $ dependencyTree initialDepth packages packageList

    getFlat packages packageList = do
      deps ← eToAff $ lmap errDep $ resolveDependencies packages packageList
      pure $ flip mkCofree [] <$> sort deps

    errDep = msg.common.brokenDependencies ∘ console.strong


type PackageStatus =
  { version ∷ Version
  , dev ∷ Boolean
  , addition ∷ Boolean
  , installation ∷ Maybe Version
  }

packageStatus
   ∷ Config
  → (PackageName × PackageInfo)
  → Aff (PackageName × PackageStatus)
packageStatus config (packageName × info) = do
  let dev = packageName `elem` fromMaybe [] config.devDependencies
         || packageName `member` fromMaybe empty config.devAdditions
      addition = packageName `member` fromMaybe empty config.additions
              || packageName `member` fromMaybe empty config.devAdditions

  tags ← catchError (listTags $ packageDir config.packageSet packageName)
                     (const $ pure [])
  pure $ packageName ×
    { version: info.version
    , dev
    , addition
    , installation: last tags
    }


type PackageNode = Cofree Array (PackageName × PackageStatus)

printTree ∷ Console → Array Boolean → Array PackageNode → Aff Unit
printTree console branches tree
  | Just {init, last} ← unsnoc tree = do
      traverse_ (printNode console $ branches <> [true]) init
      printNode console (branches <> [false]) last
  | otherwise = pure unit


printNode ∷ Console → Array Boolean → PackageNode → Aff Unit
printNode console branches leaf = do
  let (packageName × status) = C.head leaf
      showInstallation = map console.strong ∘ maybe [msg.list.notInstalled]
        \tag → guard (tag ≠ status.version) [msg.list.installed tag]
      statuses = guard status.dev [msg.list.dev]
              <> guard status.addition [msg.list.addition]
              <> showInstallation status.installation

  console.print $ showBranches
                <> packageName <> " " <> console.weak status.version
                <> guard (not $ null statuses)
                         (" (" <> intercalate " " statuses <> ")")
  printTree console branches $ C.tail leaf

  where
    showBranches
      | Just {head, tail} ← uncons branches
      , Just {init, last} ← unsnoc tail
        = console.weak $ foldMap branch init <> branchLast last
      | otherwise = ""

    branch true = "| "
    branch false = "  "

    branchLast true = "+-"
    branchLast false = "`-"
