module Pulpunte.CLI.Init
  ( InitArgs
  , init
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.String.Yarn (unlines)
import Effect.Aff (Aff)
import Effect.Aff.Util (throwError')
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists)
import Node.FS.Extra (outputFileText)
import Node.Path (FilePath, concat)
import Pulpunte.Cache (cleanCache)
import Pulpunte.Config (initConfig)
import Pulpunte.Console (Console)
import Pulpunte.Install (installAll)
import Pulpunte.Message (msg)
import Pulpunte.Purs (whichPurs)


type InitArgs =
  { force ∷ Boolean
  , skipInstall ∷ Boolean
  }

init ∷ Console → InitArgs → Aff Unit
init console { force, skipInstall } = do
  unless force $ throwErrorWhenProjectExists files
  _ ← whichPurs

  console.info msg.init.generating

  cleanCache
  config ← initConfig
  outputFiles files

  unless skipInstall $ installAll console 8 config

  console.info msg.init.done

  where
    throwErrorWhenProjectExists =
      traverse_ \{path} →
        whenM (exists $ concat path)
          $ throwError' msg.init.errExist

    outputFiles =
      traverse_ \{ path, content } →
        outputFileText UTF8 (concat path) =<< content


type FileInfo =
  { path ∷ Array FilePath
  , content ∷ Aff String
  }

files ∷ Array FileInfo
files =
  [ { path: [ "src", "Main.purs"], content: pure srcMain }
  , { path: [ "test", "Main.purs"], content: pure testMain }
  , { path: [ ".gitignore" ], content: pure gitignore }
  , { path: [ ".purs-repl" ], content: pure pursRepl }
  ]


srcMain ∷ String
srcMain = unlines
  [ "module Main where"
  , "import Prelude"
  , "import Effect (Effect)"
  , "import Effect.Console (log)"
  , ""
  , "main :: Effect Unit"
  , "main = do"
  , "  log \"Hello, World!\""
  ]

testMain ∷ String
testMain = unlines
  [ "module Test.Main where"
  , "import Prelude"
  , "import Effect (Effect)"
  , "import Effect.Console (log)"
  , ""
  , "main :: Effect Unit"
  , "main = do"
  , "  log \"You should add some tests.\""
  ]

gitignore ∷ String
gitignore = unlines
  [ "/bower_components/"
  , "/node_modules/"
  , "/output/"
  , "/generated-docs/"
  , "/.psc-package/"
  , "/.psc*"
  , "/.pulp-cache/"
  , "/.pulpunte/"
  , "/.purs*"
  , "/.psa*"
  ]

pursRepl ∷ String
pursRepl = unlines
  [ "import Prelude"
  ]
