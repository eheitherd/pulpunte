module Pulpunte.CLI.Run
  ( RunArgs
  , run
  ) where

import Prelude

import Effect.Aff (Aff)
import Pulpunte.Console (Console)
import Pulpunte.Run (RunOptions)
import Pulpunte.Run as R


type RunArgs = RunOptions

run ∷ Console → RunArgs → Aff Unit
run console args =
  let srcPaths =
        [ "src/**/*.purs"
        , ".pulpunte/*/*/src/**/*.purs"
        ]
   in R.run console args srcPaths
