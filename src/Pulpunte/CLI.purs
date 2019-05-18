module Pulpunte.CLI
  ( cli
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Aff (Aff, catchError, launchAff_, message)
import Effect.Class (liftEffect)
import Node.Process (exit)
import Node.TermSize (termSize)
import Node.Yargs (runYargs)
import Node.Yargs.Applicative (Y, flag, yarg)
import Node.Yargs.Command (CmdY, Command, cmdFlag, cmdYarg, cmdYargOptional, command, commands, requiredArg, variadicArg, (<&>))
import Node.Yargs.Setup (alias, strict, usage, wrap)
import Node.Yargs.Setup.Extra (defaultVersion, runEffectYargsSetup)
import Prelude.Unicode ((∘))
import Pulpunte.CLI.Build (build)
import Pulpunte.CLI.Default (default)
import Pulpunte.CLI.Help (help)
import Pulpunte.CLI.Init (init)
import Pulpunte.CLI.Install (install)
import Pulpunte.CLI.List (list)
import Pulpunte.CLI.Repl (repl)
import Pulpunte.CLI.Run (run)
import Pulpunte.CLI.Test (test)
import Pulpunte.CLI.Uninstall (uninstall)
import Pulpunte.Console (Console, getConsole)
import Pulpunte.Message (desc)


type GlobalOptions =
  { before ∷ String
  , else ∷ String
  , then ∷ String
  , monochrome ∷ Boolean
  }


cli ∷ Effect Unit
cli = void $ runYargs $
     usage desc.usage
  <> alias "help" "h"
  <> defaultVersion false
  <> runEffectYargsSetup (wrap ∘ toNumber ∘ _.columns <$> termSize)
  <> strict
  <> commands globalOptions
      [ defaultCmd
      , initCmd
      , installCmd
      , uninstallCmd
      , buildCmd
      , runCmd
      , testCmd
      , replCmd
      , listCmd
      , helpCmd
      ]


globalOptions ∷ Y GlobalOptions
globalOptions =
  { before: _, else: _, then: _, monochrome: _ }
    <$> yarg "before" [] Nothing (Left "") true
    <*> yarg "else" [] Nothing (Left "") true
    <*> yarg "then" [] Nothing (Left "") true
    <*> flag "monochrome" [] (Just desc.global.monochrome)


defaultCmd ∷ Command GlobalOptions
defaultCmd = command "$0" [] Nothing $
  launchCmd default $
    { version: _ }
      <$> cmdFlag "version" ["v"] (Just desc.default.version)


initCmd ∷ Command GlobalOptions
initCmd = command "init" [] (Just desc.init.command) $
  launchCmd init $
    { force: _, skipInstall: _ }
      <$> cmdFlag "force" ["f"] (Just desc.init.force)
      <*> cmdFlag "skip-install" [] (Just desc.init.skipInstall)


installCmd ∷ Command GlobalOptions
installCmd = command "install" ["i"] (Just desc.install.command) $
  launchCmd install $
    { packages: _, saveDev: _, jobs: _, clean: _ }
      <&> variadicArg "packages" (Just desc.install.packages)
      <*> cmdFlag "save-dev" ["dev", "D"] (Just desc.install.saveDev)
      <*> cmdYarg "jobs" ["j"] (Just desc.install.jobs) (Left 8) true
      <*> cmdFlag "clean" ["c"] (Just desc.install.clean)


uninstallCmd ∷ Command GlobalOptions
uninstallCmd = command "uninstall" ["u"] (Just desc.uninstall.command) $
  launchCmd uninstall $
    { packages: _ }
      <&> variadicArg "packages" Nothing


buildCmd ∷ Command GlobalOptions
buildCmd = command "build" ["b"] (Just desc.build.command) $
  launchCmd build $
    { output: _, to: _, entryModule: _, skipBundle: _, skipEntryPoint: _, watch: _, clean: _ }
      <$> cmdYarg "output" ["o"] (Just desc.build.output) (Left "output") true
      <*> cmdYargOptional "to" ["t"] (Just desc.build.to) "" true
      <*> cmdYargOptional "module" ["m"] (Just desc.build.entryModule) "" true
      <*> cmdFlag "skip-bundle" [] (Just desc.build.skipBundle)
      <*> cmdFlag "skip-entry-point" [] (Just desc.build.skipEntryPoint)
      <*> cmdFlag "watch" ["w"] (Just desc.build.watch)
      <*> cmdFlag "clean" ["c"] (Just desc.build.clean)


runCmd ∷ Command GlobalOptions
runCmd = command "run" ["r"] (Just desc.run.command) $
  launchCmd run $
    { output: _, entryModule: _, watch: _, clean: _ }
      <$> cmdYarg "output" ["o"] (Just desc.build.output) (Left "output") true
      <*> cmdYargOptional "module" ["m"] (Just desc.build.entryModule) "" true
      <*> cmdFlag "watch" ["w"] (Just desc.build.watch)
      <*> cmdFlag "clean" ["c"] (Just desc.build.clean)


testCmd ∷ Command GlobalOptions
testCmd = command "test" ["t"] (Just desc.test.command) $
  launchCmd test $
    { output: _, entryModule: _, watch: _, clean: _ }
      <$> cmdYarg "output" ["o"] (Just desc.build.output) (Left "output") true
      <*> cmdYargOptional "module" ["m"] (Just desc.build.entryModule) "" true
      <*> cmdFlag "watch" ["w"] (Just desc.build.watch)
      <*> cmdFlag "clean" ["c"] (Just desc.build.clean)


replCmd ∷ Command GlobalOptions
replCmd = command "repl" ["psci"] (Just desc.repl.command) $
  launchCmd repl $
    { clean: _ }
      <$> cmdFlag "clean" ["c"] (Just desc.build.clean)


listCmd ∷ Command GlobalOptions
listCmd = command "list" ["ls", "l"] (Just desc.list.command) $
  launchCmd list $
    { depth: _, flat: _ }
      <$> cmdYarg "depth" ["d"] (Just desc.list.depth) (Left 20) true
      <*> cmdFlag "flat" [] (Just desc.list.flat)


helpCmd ∷ Command GlobalOptions
helpCmd = command "help" ["h"] (Just desc.help.command) $
  launchCmd help $
    { command: _ }
      <&> requiredArg "command" (Just desc.help.args)


launchCmd
   ∷ ∀ a r
   . (Console → { | a} → Aff r)
  → CmdY { | a}
  → CmdY (GlobalOptions → Effect Unit)
launchCmd action = map \args options → do
  let console = getConsole options.monochrome
  launchAff_ $ catchError (action console args) \e → do
    console.error $ compressWhiteSpaces $ message e
    liftEffect $ exit 1

  where
    compressWhiteSpaces = replace (unsafeRegex "\\s+" global) " "
