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
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Purs (bundle, compile)


type BuildArgs =
  { output ∷  FilePath
  , to ∷ Maybe FilePath
  , entryModule ∷ Maybe String
  , skipBundle ∷ Boolean
  , skipEntryPoint ∷ Boolean
  , clean ∷ Boolean
  }

build ∷ Console → BuildArgs → Aff Unit
build console args = do
  let optionsForBundle
         = maybe [] (const ["--to"]) args.to
        <> maybe [] (const ["--module"]) args.entryModule

  unless (not args.skipBundle || null optionsForBundle) $
    console.warn $ msg.build.noBundle optionsForBundle

  when args.clean $ remove args.output

  flip catchError (const $ throwError' msg.build.fail) do
    let
      bundleStdout = not args.skipBundle && isNothing args.to
      srcPaths =
        [ "src/**/*.purs"
        , ".pulpunte/*/*/src/**/*.purs"
        ]

    compile { stdout: stderr, stderr } args.output srcPaths
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
