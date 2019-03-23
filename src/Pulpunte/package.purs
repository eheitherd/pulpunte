module Pulpunte.Package
  ( PackageName
  , Version
  , SpecifiedVersion(..)
  , Place
  , getLatestVersion
  , getDependencies
  ) where

import Prelude

import Control.Alternative ((<|>))
import Data.Argonaut (decodeJson)
import Data.Array (catMaybes, findLastIndex, sort, (!!))
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Aff (Aff)
import Effect.Aff.Util (eToAff, throwError')
import Foreign.Object (Object, keys)
import Node.FS.Aff (exists)
import Node.FS.Extra (readJson)
import Node.Git (URL, checkOut, clone, listTags)
import Node.Path (FilePath, concat)
import Prelude.Unicode ((∘))
import Pulpunte.Message (msg)

type PackageName = String
type Version = String


data SpecifiedVersion
  = Latest
  | SpecifiedVersion Version

type Place =
  { repo ∷ Maybe URL
  , version ∷ SpecifiedVersion
  }


getLatestVersion ∷ FilePath → PackageName → URL → Aff Version
getLatestVersion temporaryDir packageName url = do
  path ← temporaryClone temporaryDir packageName url

  tags ← listTags path
  case getLast tags of
    Just tag → pure tag
    Nothing → throwError' $ msg.package.noTag url

  where
    getLast tags = do
      reg ← hush $ regex """^v\d+\.\d+\.\d+$""" noFlags
      idx ← findLastIndex (test reg) tags
      tags !! idx


getDependencies
   ∷ FilePath
  → PackageName
  → URL
  → Version
  → Aff (Array PackageName)
getDependencies temporaryDir packageName url version = do
  path ← temporaryClone temporaryDir packageName url

  checkOut path version (Just "*.{json,dhall}")

  getDependenciesFromBower path
  <|> pure []


temporaryClone ∷ FilePath → PackageName → URL → Aff FilePath
temporaryClone tempraryDir packageName url = do
  let path = concat [ tempraryDir, packageName ]
  unlessM (exists path) $ clone path url { noCheckOut: true }
  pure path


type BowerJson =
  { dependencies ∷ Maybe (Object String)
  }

getDependenciesFromBower ∷ FilePath → Aff (Array PackageName)
getDependenciesFromBower path = do
  let jsonPath = concat [ path, "bower.json" ]
  unlessM (exists jsonPath) $ throwError' "No bower.json"
  (bowerJson ∷ BowerJson) ← eToAff ∘ decodeJson =<< readJson jsonPath
  pure $ maybe [] parseDependencies bowerJson.dependencies

  where
    parseDependencies =
      sort ∘ catMaybes ∘ (stripPrefix (Pattern "purescript-") <$> _) ∘ keys
