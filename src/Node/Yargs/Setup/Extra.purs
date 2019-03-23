module Node.Yargs.Setup.Extra
  ( defaultVersion
  , runEffectYargsSetup
  ) where


import Prelude

import Data.Function.Uncurried (runFn0, runFn1)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Yargs.Setup (YargsSetup)
import Unsafe.Coerce (unsafeCoerce)


defaultVersion ∷ Boolean → YargsSetup
defaultVersion true = unsafeCoerce \y -> runFn0 y.version
defaultVersion false = unsafeCoerce \y -> runFn1 y.version false


runEffectYargsSetup ∷ Effect YargsSetup → YargsSetup
runEffectYargsSetup effect =
  unsafeCoerce \y → unsafeCoerce (unsafePerformEffect effect) $ y
