module Pulpunte.Cache
  ( cacheRootDir
  , cleanCache
  , withTemporary
  ) where

import Prelude

import Effect.Aff (Aff, bracket)
import Node.FS.Extra (remove)
import Node.Path (FilePath, concat)


cacheRootDir ∷ FilePath
cacheRootDir = ".pulpunte"

cleanCache ∷ Aff Unit
cleanCache = remove cacheRootDir

withTemporary ∷ ∀ a. (FilePath → Aff a) → Aff a
withTemporary = bracket (remove temporaryDir $> temporaryDir) remove

temporaryDir ∷ FilePath
temporaryDir = concat [ cacheRootDir, ".tmp" ]
