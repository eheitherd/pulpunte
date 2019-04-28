module Node.Chokidar
  ( Watcher
  , watch
  , close
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn4, runEffectFn1, runEffectFn4)
import Node.Path (FilePath)


foreign import data Watcher ∷ Type

watch ∷ Array FilePath → (Watcher → FilePath → Effect Unit) → Effect Unit
watch paths handler = runEffectFn4 watchImpl paths {} "change" handler

close ∷ Watcher → Effect Unit
close = runEffectFn1 closeImpl

foreign import watchImpl
  ∷ ∀ r a.
  EffectFn4 (Array FilePath) { | r} String (Watcher → a → Effect Unit) Unit

foreign import closeImpl ∷ EffectFn1 Watcher Unit
