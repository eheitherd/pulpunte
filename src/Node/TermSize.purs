module Node.TermSize
  ( TermSize
  , termSize
  ) where

import Effect (Effect)

type TermSize =
  { columns ∷ Int
  , rows ∷  Int
  }


-- | If an error occurs, termSize will return (80, 24) instead of undefined.
foreign import termSize ∷ Effect TermSize
