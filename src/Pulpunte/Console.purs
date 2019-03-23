module Pulpunte.Console
  ( Console
  , getConsole
  ) where

import Prelude

import Ansi.Codes as AC
import Ansi.Output as AO
import Effect.Aff (Aff)
import Effect.Class.Console (error)
import Prelude.Unicode ((∘))


type Console =
  { log ∷ String → Aff Unit
  , info ∷ String → Aff Unit
  , warn ∷ String → Aff Unit
  , error ∷ String → Aff Unit
  , newline ∷ Aff Unit
  , strong ∷ String → String
  , em ∷ String → String
  }


getConsole ∷ Boolean → Console
getConsole true = monochromeConsole
getConsole false = colorConsole


colorConsole ∷ Console
colorConsole =
  { log: error
  , info: error ∘ format green
  , warn: error ∘ format yellow
  , error: error ∘ format red
  , newline
  , em
  , strong
  }


monochromeConsole ∷ Console
monochromeConsole =
  { log: error
  , info: error ∘ format monochrome
  , warn: error ∘ format monochrome
  , error: error ∘ format monochrome
  , newline
  , em: monochrome
  , strong: monochrome
  }


green ∷ String → String
green = AO.withGraphics $ AO.foreground AC.Green

yellow ∷ String → String
yellow = AO.withGraphics $ AO.foreground AC.Yellow

red ∷ String → String
red = AO.withGraphics $ AO.foreground AC.Red

monochrome ∷ String -> String
monochrome = identity

em ∷ String → String
em = AO.withGraphics $ AO.bold <> AO.foreground AC.BrightBlue

strong ∷ String → String
strong = AO.withGraphics $ AO.bold <> AO.foreground AC.BrightRed

format ∷ (String → String) → String → String
format color msg = color "* " <> msg <> "\n"

newline ∷ Aff Unit
newline = error ""
