{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Template Haskell utilities.
module Effectful.TH
  ( -- * Generate functions for dynamic effects
    makeEffect
  , makeEffectWithOptions
  , makePartialEffect
  , Options
  , defaultOptions
  , setMakeFunction
  , setMakeFunctionFor
  , setToFunctionName

    -- * Re-exports
  , Effect
  , send
  ) where

import Control.Monad (filterM, forM, unless)
import Effectful (Eff, Effect, (:>))
import Effectful.Dispatch.Dynamic (send)
import Data.Char (toLower)
import qualified Data.Map as Map
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Datatype.TyVarBndr

-- | Options for the generation of send functions.
--
-- The available options are documented in their respective setters:
--
--  * 'setMakeFunction': Select for which data constructors functions are
--    generated.
--  * 'setToFunctionName': How data constructor names are mapped to function
--    names.
data Options = Options
  { optionsMakeFunction :: Name -> Bool
  , optionsToFunctionName :: String -> String
  }

-- | Default options used by 'makeEffect'. Those are:
--
--  * Generate functions for all data constructors of the effect type.
--  * The function's name is the one of the data constructor with the first
--    letter converted to lower case.
defaultOptions :: Options
defaultOptions = Options
  { optionsMakeFunction = const True
  , optionsToFunctionName = \case
    x : xs -> toLower x : xs
    _ -> error "Empty constructor name"
  }

-- | Control which data constructor to generate functions for.
--
-- The function passed as a first argument is a predicate used to decide for
-- each constructor name whether a function is generated or not.
setMakeFunction :: (Name -> Bool) -> Options -> Options
setMakeFunction f options = options { optionsMakeFunction = f }

-- | A version of 'setMakeFunction' expecting a list of names for which
-- functions are generated.
setMakeFunctionFor :: [Name] -> Options -> Options
setMakeFunctionFor ns = setMakeFunction (`elem` ns)

-- | Controls how to map constructor names to the names of the functions that
-- are generated.
setToFunctionName :: (String -> String) -> Options -> Options
setToFunctionName f options = options { optionsToFunctionName = f }

-- | Generate functions for sending an effect operation to its handler.
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
-- > makeEffect ''Reader
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
makeEffect :: Name -> Q [Dec]
makeEffect = makeEffectWithOptions defaultOptions

-- | A version of 'makeEffect' that takes 'Options' to customize the
-- generated functions.
makeEffectWithOptions :: Options -> Name -> Q [Dec]
makeEffectWithOptions options tname = do
  tinfo <- reifyDatatype tname
  fmap mconcat $ forM (datatypeCons tinfo) $ \cinfo -> do
    if optionsMakeFunction options $ constructorName cinfo
      then makePartialEffectInfo options tinfo cinfo
      else pure []

-- | Generates the function for a particular data constructor.
--
-- > makeEffect options tname cname
-- generates a send function for the data constructor @cname@ of the type
-- @tname@.
-- This function ignores the @makeFunction@ selector of the 'Options' passed.
makePartialEffect :: Options -> Name -> Name -> Q [Dec]
makePartialEffect options tname cname = do
  tinfo <- reifyDatatype tname
  let cinfo = lookupByConstructorName cname tinfo
  makePartialEffectInfo options tinfo cinfo

-- | A version of 'makeEffect' that takes the already reified
-- 'DatatypeInfo' and 'ConstructorInfo' as arguments.
makePartialEffectInfo :: Options -> DatatypeInfo -> ConstructorInfo -> Q [Dec]
makePartialEffectInfo options tinfo cinfo = do
  let fname = mkName $ optionsToFunctionName options $ nameBase cname

  -- Check if required extensions are enabled.
  requiredExtensions (nameBase fname)
    [ FlexibleContexts
    , ScopedTypeVariables
    , TypeApplications
    ]

  let (effectVarBndrs, mBndr, rBndr) = effVars $ datatypeVars tinfo
      effectVars = map tvName effectVarBndrs
      m = tvName mBndr
      r = tvName rBndr

  es <- newName "es"
  esBndr <- kindedTV es <$> [t| [Effect] |]
  effMonad <- [t| Eff $(varT es) |]
  let replaceEff = map pure . applySubstitution (Map.singleton m effMonad)

  -- Create the function's type signature.

  -- The bindings of the type variables in the scope of the signature.
  let bndrs = map (mapTVFlag (const inferredSpec)) $
        effectVarBndrs <> constructorVars cinfo <> [esBndr, rBndr]

  -- The signature's context.
  let effect = appsT $ conT (datatypeName tinfo) : map varT effectVars
      effectConstraint = [t| $(effect) :> $(varT es) |]
      constructorConstraints = replaceEff $ constructorContext cinfo
      ctx = sequence $ effectConstraint : constructorConstraints

  -- The type of the function.
  let args = replaceEff $ constructorFields cinfo
      eff = [t| $(pure effMonad) $(varT r) |]
      funSig = arrowsT args eff

  -- The whole function signature.
  sig <- withDoc $ sigD fname $ forallT bndrs ctx funSig

  -- Create the function's definition.

  -- The names of the patterns / constructor arguments.
  ns <- let
    fieldsN = length $ constructorFields cinfo
    in mapM (\i -> newName ("arg" <> show i)) [1 .. fieldsN]

  -- The patterns and the function body.
  let pats = map varP ns
      con = conE cname
      tyApps = replaceEff $ listTVs $ constructorFields cinfo
      fields = map varE ns
      body = normalB $ [|send|] `appE` appsE (appTypesE con tyApps : fields)

  -- The whole function definition.
  defn <- funD fname [clause pats body []]

  pure [sig, defn]
  where
    cname = constructorName cinfo

    withDoc :: Q Dec -> Q Dec
#if MIN_VERSION_template_haskell(2,18,0)
    withDoc = withDecDoc $
      "-- | Send the '" <> show cname <> "' effect to the effect handler."
#else
    withDoc = id
#endif

-- | Split the type variables of the effect in
--
--  * the ones specific to the effect.
--  * the monad of the effect (i.e. the one that will be instantiated with
--    'Eff es')
--  * the return type of the operation.
--
-- For example, the effect `Reader r m a` will be split into `([r], m, a)`.
effVars :: [TyVarBndrUnit] -> ([TyVarBndrUnit], TyVarBndrUnit, TyVarBndrUnit)
effVars = go mempty
  where
    go _ [] = error "Type is no Effect !"
    go _ [_] = error "Type is no Effect !"
    go !acc [m, r] = (reverse acc, m, r)
    go !acc (tv : tvs) = go (tv : acc) tvs

-- | Extract a list of type variables in a well-scoped fashion.
--
-- See 'freeVariablesWellScoped' for further information.
listTVs :: [Type] -> [Type]
listTVs = map (VarT . tvName) . freeVariablesWellScoped

requiredExtensions :: String -> [Extension] -> Q ()
requiredExtensions what exts = do
  missing <- filterM (fmap not . isExtEnabled) exts
  let ppMissing = map (\ext -> "{-# LANGUAGE " <> show ext <> " #-}") missing
  unless (null missing) $ do
    fail $ unlines $
      [ "Generating " <> what <> " requires additional language extensions."
      , "You can enable them by adding the following pragmas to the beginning of the source file:"
      ] <> ppMissing

----------------------------------------
-- Helper functions
----------------------------------------

-- | `appsE` for `Type`.
appsT :: [Q Type] -> Q Type
appsT [] = error "appsT []"
appsT [x] = x
appsT (x:y:zs) = appsT ( (appT x y) : zs )

-- | `appsE` for type applications.
appTypesE :: Q Exp -> [Q Type] -> Q Exp
appTypesE = foldl appTypeE

-- | `appsE` for arrows.
--
-- For example, `arrows [a, b] c` will result in `a -> (b -> c)`.
arrowsT :: [Q Type] -> Q Type -> Q Type
arrowsT = flip (foldr (\arg -> appT (appT arrowT arg)))
