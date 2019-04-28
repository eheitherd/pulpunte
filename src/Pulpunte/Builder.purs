module Pulpunte.Builder
  ( runBuilder
  ) where

import Prelude

import Effect.Aff (Aff, catchError, launchAff_, message)
import Effect.Class (liftEffect)
import Node.Chokidar (close)
import Node.Chokidar as C
import Node.Path (FilePath)
import Pulpunte.Console (Console)


runBuilder
   ∷ Console
  → Boolean
  → Array FilePath
  → (Array FilePath → Aff Unit)
  → Aff Unit
runBuilder console watch srcPaths builder =
  if not watch
    then builder srcPaths
    else do
      let run = catchError (builder srcPaths) \e → console.error $ message e
          repeat = liftEffect $ C.watch srcPaths \watcher _ → launchAff_ do
            liftEffect $ close watcher
            run
            repeat
      run
      repeat
