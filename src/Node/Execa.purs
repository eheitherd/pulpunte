module Node.Execa
  ( execa
  , Options
  , Stdio
  , pipe
  , ignore
  , inherit
  , stdin
  , stdout
  , stderr
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Node.Process as P
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)


execa
   ∷ ∀ o r
   . Union o r Options
  ⇒ { | o}
  → String
  → Array String
  → Aff String
execa options cmd args =
  _.stdout <$> (toAffE $ runEffectFn3 execaImpl cmd args options)


type Options =
  ( stdin ∷ Stdio
  , stdout ∷ Stdio
  , stderr ∷ Stdio
  )

foreign import data Stdio ∷ Type

pipe ∷ Stdio
pipe = unsafeCoerce "pipe"

ignore ∷ Stdio
ignore = unsafeCoerce "ignore"

inherit ∷ Stdio
inherit = unsafeCoerce "inherit"

stdin ∷ Stdio
stdin = unsafeCoerce P.stdin

stdout ∷ Stdio
stdout = unsafeCoerce P.stdout

stderr ∷ Stdio
stderr = unsafeCoerce P.stderr


type Result =
  { stdout ∷ String
  , stderr ∷ String
  }

foreign import execaImpl ∷ ∀ o. EffectFn3
  String
  (Array String)
  o
  (Promise Result)
