module Pulpunte.Message
  ( desc
  , msg
  ) where

import Prelude

import Data.Array (length)
import Data.Foldable (intercalate)
import Data.Monoid (guard)


desc =
  { usage: "Usage: $0 <command> [args..] [options..]"
  , global:
    { before: "Run a shell command before the operation begins"
    , else: "Run a shell command if an operation failed"
    , then: "Run a shell command after the operation finishes successfully"
    , monochrome: "Don't colorize the output"
    }
  , default:
    { version: "Show current version"
    }
  , init:
    { command: "Generate a project skeleton."
    , force: "Overwrite existing files"
    , skipInstall: "Don't install packages."
    }
  , install:
    { command: "Install packages from repositories."
    , packages: intercalate "\n"
        [ "Available expressions are ..."
        , "a package name (e.g. \"arrays\")"
        , "a Github account and a package name (e.g. \"purescript/arrays\")"
        , "a URL (e.g. \"https://github.com/purescript/purescript-arrays.git\")"
        , "with a version (e.g. \"arrays@v5.2.1\", \"purescript/arrays@latest\")"
        ]
    , saveDev: "Save installed packages into devDependencies or devAdditions"
    , jobs: "Parallelize jobs with specific number."
    }
  , uninstall:
    { command: "Uninstall packages from the project."
    }
  , build:
    { command: "Compile PureScript source files and bundle compiled files."
    , output: "The output directory (default: \"output\")"
    , to: "The output .js file. If not specified, the output goes to stdout."
    , entryModule: "Entry point module name (default: \"Main\")"
    , skipBundle: "Don't bundle compiled files after compiling"
    , skipEntryPoint: "Don't generate code to run the main method in the entry module"
    , watch: "Watch the source files for changes"
    , clean: "Delete all compiled files before compiling"
    }
  , run:
    { command: "Build and run the project."
    }
  , test:
    { command: "Run project tests."
    }
  , repl:
    { command: "Enter the interactive mode (PSCi)"
    }
  }


msg =
  { init:
    { generating: "Generating project skeleton..."
    , done: "Done."
    , errExist:
           "There's already a project here. "
        <> "Run `pulpunte init --force` "
        <> "if you're sure you want to overwrite it."
    }
  , install:
    { currentPackageSet: \url version →
           "Package set: " <> version <> " (" <> url <> ")"
    , installing: \package → "Installing " <> package
    , done: \(num ∷ Int) →
           show num
        <> " package"
        <> (if num == 1 then " has" else "s have")
        <> " been installed."
    , ignoreSaveDev:
           "Option `--save-dev` will be ignored,"
        <> " because no packages are specified."
    , alreadyInstalled: \packages →
           "Package "
        <> intercalate ", " packages
        <> " exist" <> guard (length packages == 1) "s"
        <> " in dependencies."
    , addedToDeps: \packages →
           "Package "
        <> intercalate ", " packages
        <> " " <> have packages
        <> " been added to dependencies."
    , errDuplicated: \packages →
           "Package "
        <> intercalate ", " packages
        <> (if length packages == 1 then " is" else " are")
        <> " duplicated."
    , errNotExistInPackaeSet: \package →
           "Package " <> package
        <> " does not exist in package set."
    , errDepNotExistInPackageSet: \package →
           "Package " <> package <> " is depended on by another package"
        <> " but not present in package set or additions."
    }
  , uninstall:
    { removedFromDeps: \packages →
           "Package "
        <> intercalate ", " packages
        <> " " <> have packages
        <> " been removed from dependencies."
    , uninstalling: \package → "Uninstalling " <> package
    , done: \(num ∷ Int) →
           show num
        <> " package" <>  (if num == 1 then " has" else "s have")
        <> " been uninstalled."
    , brokenDependencies: \package →
           "Package " <> package <> " is depended on by another package"
        <> " but not present in package set or additions."
    , errNotSpecified: "Pass the package names you want to uninstall."
    , errNotExistInDeps: \packages →
           "Package "
        <> intercalate ", " packages
        <> " do" <> guard (length packages == 1) "es"
        <> " not exist in dependencies."
    , errStillNeeded: \package →
           "Could not uninstall package " <> package
        <> " because another package depends on it."
    }
  , build:
    { bundling: \output → "Bundling into " <> output
    , bundlingStdout: "Bundling..."
    , noBundle: \(options ∷ Array String) →
           "Option `"
        <> intercalate "`, `" options
        <> "` will be ignored,"
        <> " because `--skip-bundle` is specified."
    , fail: "Build failed."
    , done: "Build succeeded."
    }
  , test:
    { ok: "Tests OK."
    , ng: "Tests NG."
    }
  , config:
    { errNotExist:
           "Could not find a pulpunte.json file in the current directory."
        <> " You can generate it using `pulpunte init`."
    }
  , packageSet:
    { errGetPackage: \packageName →
        "Could not get package " <> packageName <> " from the repository."
    }
  , package:
    { noTag: \url →
        "No tag in " <> url
    }
  }


have ∷ Array String → String
have list = if length list == 1 then "has" else "have"
