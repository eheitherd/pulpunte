module Node.Which
  ( which
  ) where


import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Node.Path (FilePath)
import Prelude.Unicode ((∘))


which ∷ String → Aff FilePath
which = toAffE ∘ runEffectFn1 whichImpl


foreign import whichImpl ∷ EffectFn1 String (Promise FilePath)
