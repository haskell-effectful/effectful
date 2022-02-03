{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell utilities.
module Effectful.TH
  ( -- * Generate functions for dynamic effects
    makeSendFunctions

    -- * Re-exports
  , Effect
  , send
  ) where

import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Data.Char
import Data.Generics
import qualified Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Datatype.TyVarBndr

-- | Generate functions for sending a dynamic effect to the effect handler.
--
-- For example,
--
-- > -- These extensions are needed for the definition of the effect.
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE GADTs #-}
-- > {-# LANGUAGE TypeFamilies #-}
-- > -- These extensions are needed for the generation of the functions.
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE ScopedTypeVariables #-}
-- > {-# LANGUAGE TemplateHaskell #-}
-- > {-# LANGUAGE TypeApplications #-}
-- > module MyReader where
-- >
-- > import Effectful (Dispatch(Dynamic), DispatchOf)
-- > import Effectful.TH
-- >
-- > data Reader r :: Effect where
-- >   Ask   :: Reader r m r
-- >   Local :: (r -> r) -> m a -> Reader r m a
-- >
-- > type instance DispatchOf (Reader r) = 'Dynamic
-- >
-- > makeSendFunctions ''Reader
--
-- will generate the following functions:
--
-- > ask :: forall (r :: Type) (es :: [Effect]) (b :: Type).
-- >   (Reader r :> es, r ~ b) => Eff es b
-- > ask = send Ask
-- >
-- > local :: forall (r :: Type) (es :: [Effect]) (b :: Type).
-- >   Reader r :> es => (r -> r) -> Eff es b -> Eff es b
-- > local arg1 arg2 = send (Local @r @(Eff es) @b arg1 arg2)
makeSendFunctions :: Name -> Q [Dec]
makeSendFunctions tname = do
  tinfo <- reifyDatatype tname
  fmap mconcat $ mapM (makeSendFunctionFor tinfo) $ datatypeCons tinfo

-- | Generates the function for a particular data constructor.
makeSendFunctionFor :: DatatypeInfo -> ConstructorInfo -> Q [Dec]
makeSendFunctionFor tinfo cinfo = do
  let fname = toFunctionName $ constructorName cinfo

  let (effectVarBndrs, mBndr, rBndr) = effVars $ datatypeVars tinfo
      effectVars = map tvName effectVarBndrs
      m = tvName mBndr
      r = tvName rBndr

  es <- newName "es"
  esBndr <- kindedTV es <$> [t| [Effect] |]
  effMonad <- [t| Eff $(varT es) |]
  let replaceEff = map (pure . replaceTV m effMonad)

  -- Create the function's type signature.
  let bndrs = effectVarBndrs <> constructorVars cinfo <> [esBndr, rBndr]

  let effect = appsT $ conT (datatypeName tinfo) : map varT effectVars
      effectConstraint = [t| $(effect) :> $(varT es) |]
      constructorConstraints = replaceEff $ constructorContext cinfo
      ctx = sequence $ effectConstraint : constructorConstraints

  let args = replaceEff $ constructorFields cinfo
      eff = [t| Eff $(varT es) $(varT r) |]
      funSig = arrowsT args eff

  sig <- sigD fname $ forallT bndrs ctx funSig

  -- Create the function's definition.
  ns <- let
    fieldsN = length $ constructorFields cinfo
    in mapM (\i -> newName ("arg" <> show i)) [1 .. fieldsN]

  let pats = map varP ns
      con = conE $ constructorName cinfo
      tyApps = replaceEff $ listTyVars $ constructorFields cinfo
      fields = map varE ns
      body = normalB $ [|send|] `appE` appsE (appTypesE con tyApps : fields)

  defn <- funD fname [clause pats body []]

#if MIN_VERSION_template_haskell(2,18,0)
  let doc :: String
      doc = "-- | Send the '"
        <> show (constructorName cinfo)
        <> "' effect to the effect handler."
  pure [withDecDoc doc sig, defn]
#else
  pure [sig, defn]
#endif

toFunctionName :: Name -> Name
toFunctionName cname = let
  x : xs = nameBase cname
  in mkName $ toLower x : xs

effVars :: [TyVarBndrUnit] -> ([TyVarBndr], TyVarBndr, TyVarBndr)
effVars = go mempty
  where
    go _ [] = error "Type is no Effect !"
    go _ [_] = error "Type is no Effect !"
    go acc [m, r] = (reverse acc, m, r)
    go acc (tv : tvs) = go (tv : acc) tvs

listTyVars :: [Type] -> [Type]
listTyVars = foldl f mempty
  where
    f memo t = let
      tvs = listify isTyVar t
      in memo <> (Data.List.nub tvs Data.List.\\ memo)

    isTyVar VarT{} = True
    isTyVar _ = False

replaceTV :: Name -> Type -> Type -> Type
replaceTV n r = everywhere (mkT f)
  where
    f :: Type -> Type
    f (VarT tv) | tv == n = r
    f t = t

----------------------------------------
-- Helper functions
----------------------------------------

appsT :: [Q Type] -> Q Type
appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT ( (appT x y) : zs )

appTypesE :: Q Exp -> [Q Type] -> Q Exp
appTypesE = foldl appTypeE

arrowsT :: [Q Type] -> Q Type -> Q Type
arrowsT = flip (foldr (\arg -> appT (appT arrowT arg)))
