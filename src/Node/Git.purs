module Node.Git
  ( URL
  , clone
  , cloneShallow
  , checkOut
  , listTags
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.Path (FilePath)
import Prim.Row (class Union)
import Record (union)


type URL = String

type CloneOptions =
  ( ref ∷ String
  , singleBranch ∷ Boolean
  , noCheckOut ∷ Boolean
  , noGitSuffix ∷ Boolean
  , depth ∷ Int
  )

clone
   ∷ ∀ o r
   . Union o r CloneOptions
  ⇒ FilePath
  → URL
  → Record o
  → Aff Unit
clone dir url options =
  toAffE $ runEffectFn2 gitImpl "clone" $ union { dir, url } options


cloneShallow ∷ FilePath → URL → String → Aff Unit
cloneShallow dir url ref =
  clone dir url
    { ref
    , singleBranch: true
    , noGitSuffix: true
    , depth: 1
    }


checkOut ∷ FilePath → String → Maybe String → Aff Unit
checkOut dir ref glob =
  toAffE $ runEffectFn2 gitImpl "checkout"
    { dir
    , ref
    , pattern: toNullable glob
    }


listTags ∷ FilePath → Aff (Array String)
listTags dir = toAffE $ runEffectFn2 gitImpl "listTags" { dir }


foreign import gitImpl ∷ ∀ a r. EffectFn2 String a (Promise r)
