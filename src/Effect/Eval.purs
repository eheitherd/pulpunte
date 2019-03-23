module Effect.Eval
  ( eval
  ) where


import Effect (Effect)
import Foreign (Foreign)

foreign import eval ∷ String → Effect Foreign
