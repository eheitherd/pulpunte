module Pulpunte.CLI.Test
  ( TestArgs
  , test
  ) where

import Prelude

import Control.Alternative ((<|>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, catchError, message)
import Effect.Aff.Util (throwError')
import Pulpunte.Builder (runBuilder)
import Pulpunte.Console (Console)
import Pulpunte.Message (msg)
import Pulpunte.Run (RunOptions, run)


type TestArgs = RunOptions

test ∷ Console → TestArgs → Aff Unit
test console args =
  let srcPaths =
        [ "src/**/*.purs"
        , "test/**/*.purs"
        , ".pulpunte/*/*/src/**/*.purs"
        ]

   in runBuilder console args.watch srcPaths \paths →
    flip catchError ng do
      let options = args { entryModule = args.entryModule <|> Just "Test.Main" }

      run console options srcPaths

      console.newline
      console.info msg.test.ok

    where
      ng error = do
        console.log $ message error
        console.newline
        throwError' msg.test.ng
