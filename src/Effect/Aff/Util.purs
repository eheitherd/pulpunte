module Effect.Aff.Util
  ( throwError'
  , eToAff
  , mapErrorMsg
  ) where

import Prelude

import Data.Either (Either, either)
import Effect.Aff (Aff, catchError, error, message, throwError)
import Prelude.Unicode ((∘))


throwError' ∷ ∀ a. String → Aff a
throwError' = throwError ∘ error


eToAff ∷ ∀ a. Either String a → Aff a
eToAff = either throwError' pure


mapErrorMsg ∷ ∀ a. (String → String) → Aff a → Aff a
mapErrorMsg f = flip catchError $ throwError' ∘ f ∘ message
