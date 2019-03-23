module Pulpunte.Config
  ( Config
  , initConfig
  , readConfig
  , writeConfig
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (catchJust, throwError)
import Data.Argonaut (decodeJson, encodeJson, (:=))
import Data.Maybe (Maybe(..))
import Data.String.Utils (startsWith)
import Effect.Aff (Aff, error, message)
import Effect.Aff.Util (eToAff)
import Effect.Class (liftEffect)
import Foreign.Object (Object, empty, fromFoldable)
import Node.FS.Extra (readJson, writeJson)
import Node.Path (FilePath, basename)
import Node.Process (cwd)
import Prelude.Unicode ((∘))
import Pulpunte.Message (msg)
import Pulpunte.PackageSet (PackageInfo, PackageSet)
import Pulpunte.Purs (version)


pulpunteJson ∷ FilePath
pulpunteJson = "pulpunte.json"

packageJson ∷ FilePath
packageJson = "package.json"

type Config =
  { name ∷ Maybe String
  , packageSet ∷ PackageSet
  , dependencies ∷ Array String
  , additions ∷ Maybe (Object PackageInfo)
  }


initConfig ∷ Aff Config
initConfig = do
  name <- readProjectName <|> getCurrentDir <|> pure Nothing
  pursVersion ← version
  let config = defaultConfig name pursVersion
  writeConfig config
  pure config

  where
    readProjectName = eToAff ∘ decodeJson =<< readJson packageJson
    getCurrentDir = Just <$> basename <$> liftEffect cwd


readConfig ∷ Aff Config
readConfig = eToAff ∘ decodeJson
              =<< catchJust replaceError (readJson pulpunteJson) throwError
  where
    replaceError e =
      if startsWith "ENOENT:" (message e)
        then Just $ error msg.config.errNotExist
        else Nothing


writeConfig ∷ Config → Aff Unit
writeConfig config = writeJson 2 pulpunteJson $ encodeJson $
  fromFoldable
    [ "name" := config.name
    , "packageSet" := config.packageSet
    , "dependencies" := config.dependencies
    , "additions" := config.additions
    ]


defaultConfig ∷ (Maybe String) → String → Config
defaultConfig name pursVersion =
  { name
  , packageSet:
      { repo: "https://github.com/purescript/package-sets.git"
      , set: "psc-" <> pursVersion
      }
  , dependencies:
      [ "console"
      , "effect"
      , "prelude"
      ]
  , additions: Just empty
  }
