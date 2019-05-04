module Pulpunte.CLI.Help
  ( help
  ) where

import Prelude

import Effect.Aff (Aff)
import Node.Execa (execa, stderr, stdout)
import Pulpunte.Console (Console)


type HelpArgs =
  { command ∷ String
  }

help ∷ Console → HelpArgs → Aff Unit
help _ { command } =
  void $ execa {stdout, stderr} "pulpunte" [command, "-h"]
