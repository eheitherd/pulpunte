module Node.Yargs.Command
  ( Command
  , command
  , commands
  , CmdY
  , cmdYarg
  , cmdFlag
  , cmdRest
  , cmdYargOptional
  , ArgY
  , requiredArg
  , OptY
  , optionalArg
  , variadicArg
  , class TransY
  , cmdY
  , mapToCmdY
  , applyToCmdY
  , (<&>)
  , (<~>)
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..), either, hush)
import Data.Foldable (foldMap, traverse_)
import Data.Function.Uncurried (runFn2, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (F, Foreign, renderForeignError)
import Node.Yargs.Applicative (class Arg, Y, arg, flag, rest, yarg)
import Node.Yargs.Setup (YargsSetup)
import Prelude.Unicode ((∘))
import Unsafe.Coerce (unsafeCoerce)

newtype Command g = Command (Y g → YargsSetup)


command
   ∷ ∀ g
   . String
  → Array String
  → Maybe String
  → CmdY (g → Effect Unit)
  → Command g
command cmd aliases desc (CmdY cmdy) =
  let
    cmd' = [ cmd <> foldMap (" " <> _) cmdy.cmdArgs ] <> aliases
    desc' = toDesc desc
    ycmd' = fromY cmdy.y
    handler read value
      = unsafePerformF $ ycmd'.read value <*> read value
  in
    Command \g →
      let read = (fromY g).read
       in unsafeCoerce \y → runFn4 y.command cmd' desc' ycmd'.setup (handler read)

  where
    unsafePerformF f = unsafePerformEffect $
      either (traverse_ $ error ∘ renderForeignError)
             identity
             $ runExcept f


commands ∷ ∀ g. Y g → Array (Command g) → YargsSetup
commands y cs = (fromY y).setup <> foldMap (\(Command cmd) → cmd y) cs


foreign import data Desc ∷ Type

toDesc ∷ Maybe String → Desc
toDesc = case _ of
  Just str → unsafeCoerce str
  _ → unsafeCoerce false

--

newtype CmdY a = CmdY
  { cmdArgs ∷ Array String
  , y ∷ Y a
  }

instance functorCmdY ∷ Functor CmdY where
  map f (CmdY a) = CmdY { cmdArgs: a.cmdArgs, y: f <$> a.y }

instance applyCmdY ∷ Apply CmdY where
  apply (CmdY a) (CmdY b) = CmdY { cmdArgs: a.cmdArgs <> b.cmdArgs, y: a.y <*> b.y }

instance applicativeCmdY ∷ Applicative CmdY where
  pure a = CmdY { cmdArgs: mempty, y: pure a }


toCmd ∷ Y ~> CmdY
toCmd y = CmdY { cmdArgs: mempty, y }

cmdYarg
   ∷ ∀ a
   . Arg a
  ⇒ String
  → Array String
  → Maybe String
  → Either a String
  → Boolean
  → CmdY a
cmdYarg key aliases desc required needArg
  = toCmd $ yarg key aliases desc required needArg

cmdFlag ∷ String → Array String → Maybe String → CmdY Boolean
cmdFlag key aliases desc = toCmd $ flag key aliases desc

cmdRest ∷ CmdY (Array Foreign)
cmdRest = toCmd rest

cmdYargOptional
   ∷ ∀ a
   . Arg a
  ⇒ Eq a
  ⇒ String
  → Array String
  → Maybe String
  → a
  → Boolean
  → CmdY (Maybe a)
cmdYargOptional key aliases desc nothing needArg
  = toMaybe nothing <$> cmdYarg key aliases desc (Left nothing) needArg

  where
    toMaybe n a = if n == a then Nothing else Just a

--

positionalArg
   ∷ ∀ a
   . String
  → String
  → String
  → Maybe String
  → (Foreign → F a)
  → CmdY a
positionalArg key bracketL bracketR desc read = CmdY
  let
    setup = case desc of
      Just describe → unsafeCoerce \y
                        → runFn2 y.positional key { type: "string", describe }
      Nothing → mempty
  in
    { cmdArgs: [ bracketL <> key <> bracketR ]
    , y: toY { setup, read }
    }

--

newtype ArgY a = ArgY (CmdY a)

instance functorArgY ∷ Functor ArgY where
  map f (ArgY a) = ArgY $ f <$> a

instance applyArgY ∷ Apply ArgY where
  apply (ArgY a) (ArgY b) = ArgY $ a <*> b

instance applicativeArgY ∷ Applicative ArgY where
  pure = ArgY ∘ pure


-- If desc is Nothing, passing a number will cause an error.
requiredArg ∷ String → Maybe String → ArgY String
requiredArg key desc = ArgY $
  positionalArg key "<" ">" desc (fromY $ arg key).read

--

newtype OptY a = OptY (CmdY a)

-- If desc is Nothing, passing a number will cause an error.
optionalArg ∷ String → Maybe String → OptY (Maybe String)
optionalArg key desc = OptY $
  positionalArg key "[" "]" desc $
    map hush ∘ try ∘ (fromY $ arg key).read

-- If desc is Nothing, passing a number will cause an error.
variadicArg ∷ String → Maybe String → OptY (Array String)
variadicArg key desc = OptY $
  positionalArg key "[" "..]" desc $
    map (either (const []) identity) ∘ try ∘ (fromY $ arg key).read

--

class TransY (t ∷ Type → Type) where
  cmdY ∷ ∀ a. t a → CmdY a

instance transYargY ∷ TransY ArgY where
  cmdY (ArgY y) = y

instance transYoptY ∷ TransY OptY where
  cmdY (OptY y) = y

mapToCmdY ∷ ∀ a b t. TransY t ⇒ (a → b) → t a → CmdY b
mapToCmdY f t = f <$> cmdY t

infixl 4 mapToCmdY as <&>

applyToCmdY ∷ ∀ a b t. TransY t ⇒ ArgY (a → b) → t a → CmdY b
applyToCmdY (ArgY y) t = y <*> cmdY t

infixl 4 applyToCmdY as <~>

--

toY ∷ ∀ a. { setup ∷ YargsSetup, read ∷ Foreign -> F a } → Y a
toY = unsafeCoerce

fromY ∷ ∀ a. Y a → { setup ∷ YargsSetup, read ∷ Foreign -> F a }
fromY = unsafeCoerce
