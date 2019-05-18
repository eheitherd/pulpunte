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
  let
    srcPath = (_ <> "/**/*.purs")
    srcPaths' =  srcPath <$> srcPaths
  in
    if not watch
      then builder srcPaths'
      else do
        let watchPaths = ([srcPath, (_ <> "/**/*.js")] <@> _) =<< srcPaths
            run = catchError (builder srcPaths') \e → console.error $ message e
            repeat = liftEffect $ C.watch watchPaths \watcher _ → launchAff_ do
              liftEffect $ close watcher
              run
              repeat
        run
        repeat
