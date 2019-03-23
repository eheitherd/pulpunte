module Pulpunte.Purs
  ( version
  , compile
  , BundleOptions
  , bundle
  ) where

import Prelude

import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Monoid (guard)
import Effect.Aff (Aff)
import Node.Execa (Options, execa)
import Node.Path (FilePath, concat)
import Prim.Row (class Union)


version ∷ Aff String
version = execa {} "purs" ["--version"]


compile
   ∷ ∀ o r
   . Union o r Options
  ⇒ { | o}
  → FilePath
  → Array FilePath
  → Aff Unit
compile options output srcPaths = do
  let
    args =
        [ "compile" ]
      <> srcPaths
      <> ["--output", output]

  void $ execa options "purs" args


type BundleOptions =
  { output ∷ Maybe FilePath
  , entryModule ∷ Maybe String
  , entryPoint ∷ Boolean
  }

bundle
   ∷ ∀ o r
   . Union o r Options
  ⇒ { | o}
  → FilePath
  → BundleOptions
  → Aff String
bundle options jsPath bundleOptions = do
  let entryModule = fromMaybe "Main" bundleOptions.entryModule
      args =
        [ "bundle"
        , concat [ jsPath, "*", "*.js" ]
        , "--module", entryModule
        ]
        <> guard bundleOptions.entryPoint [ "--main", entryModule ]
        <> maybe [] (\o → [ "-o", o]) bundleOptions.output

  execa options "purs" args
