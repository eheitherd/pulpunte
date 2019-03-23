module Pulpunte.CLI.Default
  ( DefaultArgs
  , default
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Node.Execa (execa)
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
        log $ "Pulpunte version " <> package.version
        pursVersion ← P.version
        log $ "purs version " <> pursVersion

      showHelp = log =<< execa {} "pulpunte" ["-h"]
