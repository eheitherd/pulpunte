module Pulpunte.Run
  ( RunOptions
  , run
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Util (mapErrorMsg)
import Effect.Class (liftEffect)
import Effect.Eval (eval)
import Node.Execa (stderr)
import Node.FS.Extra (remove)
import Node.Path (FilePath)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Purs (bundle, compile, whichPurs)


type RunOptions =
  { output ∷  FilePath
  , entryModule ∷ Maybe String
  , watch ∷ Boolean
  , clean ∷ Boolean
  }

run ∷ Console → RunOptions → Array FilePath → Aff Unit
run console options srcPaths = do
  _ ← whichPurs

  when options.clean $ remove options.output

  output ← mapErrorMsg (const msg.build.fail) do
    compile { stdout: stderr, stderr } options.output srcPaths
    console.newline

    bundle { stderr } options.output
      { output: Nothing
      , entryModule: options.entryModule
      , entryPoint: true
      }

  liftEffect $ void $ eval output
