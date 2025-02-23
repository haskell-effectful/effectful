-- Most tests copied from polysemy-plugin:
--
-- https://github.com/polysemy-research/polysemy/tree/master/polysemy-plugin/test
--
-- (c) 2019 Sandy Maguire, licensed under BSD-3-Clause
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-foralls -fplugin=Effectful.Plugin #-}
module Main where

import Data.String
import Unsafe.Coerce

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Labeled
import Effectful.Labeled.Reader
import Effectful.State.Static.Local

main :: IO ()
main = pure ()

----------------------------------------
-- Tests

data Function i o :: Effect where
  Call :: i -> Function i o m o
type instance DispatchOf (Function i o) = Dynamic

call :: (HasCallStack, Function i o :> es) => i -> Eff es o
call a = send $ Call a

callTest
  :: ( Function Int a :> es
     , Function a Int :> es
     , Labeled "x" (Reader Int) :> es
     , Labeled "y" (Reader b) :> es
     , IsString s
     , Function s a :> es
     )
  => Eff es ()
callTest = do
  a1 <- call 1
  a2 <- call ""
  _ <- call a1
  _ <- call a2
  (_::Int) <- ask
  _ <- ask @"y"
  pure ()

class X a where
  xxx :: a

class MPTC a b where
  mptc :: a -> b

instance MPTC Bool Int where
  mptc _ = 1000

ordPut :: (State s :> es, Ord s) => s -> Eff es ()
ordPut = put

uniquelyX :: (X a, State a :> es) => Eff es ()
uniquelyX = put xxx

uniquelyA :: (Num a, State a :> es, State b :> es, IsString b) => Eff es ()
uniquelyA = put 10 >> put ""

uniquelyInt :: (State Int :> es, State String :> es) => Eff es ()
uniquelyInt = ordPut 10 >> put ""

uniquelyString :: (State Int :> es, State String :> es) => Eff es ()
uniquelyString = put mempty

uniquelyB :: (MPTC Bool b, State String :> es, State b :> es) => Eff es ()
uniquelyB = put $ mptc False

uniquelyState' :: (Error () :> es, State () :> es) => Eff es ()
uniquelyState' = pure ()

idState :: State s :> es => Eff es ()
idState = do
  s <- get
  put s

intState :: State Int :> es => Eff es ()
intState = put 10

numState :: Num a => State a :> es => Eff es ()
numState = put 10

strState :: State String :> es => Eff es ()
strState = put "Hello"

oStrState :: IsString a => State a :> es => Eff es ()
oStrState = put "hello"

err :: Error e :> es => Eff es Bool
err =
  catchError
    (throwError_ (error ""))
    (\_ _ -> pure True)

errState :: (Num s, Error e :> es, State s :> es) => Eff es Bool
errState = do
  numState
  err

newtype MyString = MyString String
  deriving newtype (IsString, Eq, Show)

data Janky = forall s. Janky (forall _i. Eff '[State s] ())

jankyState :: Janky
jankyState = Janky $ put True -- The plugin disambiguates effects for concrete rows too

unsafeUnjank :: Janky -> Eff '[State Bool] ()
unsafeUnjank (Janky m) = unsafeCoerce m

data MoreJanky = forall y. MoreJanky (MPTC Bool y => Eff [State (Bool, y), State (Char, y)] ())

mptcGet :: MPTC x Bool => x
mptcGet = undefined

moreJankyState :: MoreJanky
moreJankyState = MoreJanky $ put (mptcGet, True)

data TaggedState k s :: Effect where
  TaggedGet :: forall k s m. TaggedState k s m s
  TaggedPut :: forall k s m. s -> TaggedState k s m ()
type instance DispatchOf (TaggedState k s) = Dynamic

runTaggedState :: s -> Eff (TaggedState k s : es) a -> Eff es (a, s)
runTaggedState s = reinterpret_ (runState s) $ \case
  TaggedGet    -> get
  TaggedPut s' -> put s'

test :: (TaggedState Char Int :> es, TaggedState Bool Int :> es) => Eff es ()
test = do
  send $ TaggedPut @Bool 10
  send $ TaggedPut @Char (-10)

newtype Select a = Select a

data DBAction whichDb :: Effect where
  DoSelect :: Select a -> DBAction whichDb m (Maybe a)
type instance DispatchOf (DBAction whichDb) = Dynamic

runDBAction :: Eff (DBAction which : es) a -> Eff es a
runDBAction = interpret_ $ \case
  DoSelect (Select a) -> pure $ Just a
