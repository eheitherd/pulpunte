module Pulpunte.CLI.Repl
  ( repl
  ) where

import Prelude

import Effect.Aff (Aff)
import Node.FS.Extra (remove)
import Node.Path (FilePath)
import Pulpunte.Console (Console)
import Pulpunte.Purs as P


type ReplArgs =
  { clean ∷ Boolean
  }

psciDir ∷ FilePath
psciDir = ".psci_modules"


repl ∷ Console → ReplArgs → Aff Unit
repl console { clean } = do
  when clean $ remove psciDir

  P.repl
    [ "src/**/*.purs"
    , ".pulpunte/*/*/src/**/*.purs"
    ]
