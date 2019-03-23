module Node.FS.Extra
  ( outputFileText
  , readJson
  , writeJson
  , remove
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Node.Encoding (Encoding)
import Node.Path (FilePath)
import Prelude.Unicode ((∘))


outputFileText ∷ Encoding → FilePath → String → Aff Unit
outputFileText encoding filePath content
  = toAffE $ runEffectFn3 outputFileImpl encoding filePath content

readJson ∷ ∀ a. FilePath → Aff a
readJson = toAffE ∘ runEffectFn1 readJsonImpl

writeJson ∷ ∀ a. Int → FilePath → a → Aff Unit
writeJson spaces filePath object
  = toAffE $ runEffectFn3 writeJsonImpl spaces filePath object

remove ∷ FilePath → Aff Unit
remove = toAffE ∘ runEffectFn1 removeImpl

foreign import outputFileImpl ∷ EffectFn3
  Encoding
  FilePath
  String
  (Promise Unit)

foreign import readJsonImpl ∷ ∀ a. EffectFn1 FilePath (Promise a)

foreign import writeJsonImpl ∷ ∀ a. EffectFn3 Int FilePath a (Promise Unit)

foreign import removeImpl ∷ EffectFn1 FilePath (Promise Unit)
