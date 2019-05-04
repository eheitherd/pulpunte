module Pulpunte.CLI.Default
  ( DefaultArgs
  , default
  ) where

import Prelude

import Effect.Aff (Aff, catchError, message)
import Node.Execa (execa, stderr, stdout)
import Prelude.Unicode ((∘))
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
        console.print $ "pulpunte v" <> package.version
        catchError (void P.whichPurs) (console.warn ∘ message)
        pursVersion ← P.version
        console.print $ "purs v" <> pursVersion

      showHelp = void $ execa {stdout, stderr} "pulpunte" ["-h"]
