module Effect.Aff.Util
  ( throwError'
  , eToAff
  ) where

import Prelude

import Data.Either (Either, either)
import Effect.Aff (Aff, error, throwError)
import Prelude.Unicode ((∘))


throwError' ∷ ∀ a. String → Aff a
throwError' = throwError ∘ error


eToAff ∷ ∀ a. Either String a → Aff a
eToAff = either throwError' pure
