module Pulpunte.CLI.Build
  ( BuildArgs
  , build
  ) where

import Prelude

import Data.Array (null)
import Data.Maybe (Maybe, isNothing, maybe)
import Effect.Aff (Aff, catchError)
import Effect.Aff.Util (throwError')
import Node.Execa (stderr, stdout)
import Node.FS.Extra (remove)
import Node.Path (FilePath)
import Prelude.Unicode ((∘))
import Pulpunte.Builder (runBuilder)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Purs (bundle, compile, whichPurs)


type BuildArgs =
  { output ∷  FilePath
  , to ∷ Maybe FilePath
  , entryModule ∷ Maybe String
  , skipBundle ∷ Boolean
  , skipEntryPoint ∷ Boolean
  , watch ∷ Boolean
  , clean ∷ Boolean
  }

build ∷ Console → BuildArgs → Aff Unit
build console args = do
  _ ← whichPurs

  let optionsForBundle
         = maybe [] (const ["--to"]) args.to
        <> maybe [] (const ["--module"]) args.entryModule

  unless (not args.skipBundle || null optionsForBundle) $
    console.warn $ msg.build.noBundle optionsForBundle

  when args.clean $ remove args.output

  let srcPaths =
        [ "src/**/*.purs"
        , ".pulpunte/*/*/src/**/*.purs"
        ]

  runBuilder console args.watch srcPaths \paths →
    flip catchError (\_ → throwError' msg.build.fail) do
      compile { stdout: stderr, stderr } args.output paths
      console.newline

      unless args.skipBundle do
        console.info $ maybe msg.build.bundlingStdout
                            (msg.build.bundling ∘ console.em) args.to

        _ ← bundle { stdout, stderr } args.output
          { output: args.to
          , entryModule: args.entryModule
          , entryPoint: not args.skipEntryPoint
          }

        when (isNothing args.to) console.newline

      console.info msg.build.done
