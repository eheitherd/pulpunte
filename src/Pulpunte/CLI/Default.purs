module Pulpunte.CLI.Default
  ( DefaultArgs
  , default
  ) where

import Prelude

import Effect.Aff (Aff)
import Node.Execa (execa, stderr, stdout)
import Pulpunte.Console (Console)
import Pulpunte.NpmPackage (package)
import Pulpunte.Purs as P


type DefaultArgs =
  { version ∷ Boolean
  }

default ∷ Console → DefaultArgs → Aff Unit
default console { version } =
  if version
    then showVersion
    else showHelp

    where
      showVersion = do
        console.print $ "Pulpunte version " <> package.version
        pursVersion ← P.version
        console.print $ "purs version " <> pursVersion

      showHelp = void $ execa {stdout, stderr} "pulpunte" ["-h"]
