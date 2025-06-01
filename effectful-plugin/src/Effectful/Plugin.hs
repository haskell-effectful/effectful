{-# LANGUAGE CPP #-}
module Effectful.Plugin (plugin) where

import Data.Either
import Data.Foldable
import Data.IORef
import Data.Maybe
import GHC.Core.Class
import GHC.Core.InstEnv
import GHC.Core.Predicate
import GHC.Core.TyCo.Rep
import GHC.Core.TyCo.Subst
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Unify
import GHC.Driver.Env
import GHC.Driver.Plugins
import GHC.Tc.Plugin
import GHC.Tc.Types
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.TcType
import GHC.Types.Name
import GHC.Types.Unique.FM
import GHC.Types.Unique.Set
import GHC.Types.Var.Set
import GHC.Unit.Finder
import GHC.Unit.Module
import GHC.Utils.Outputable qualified as O

#if __GLASGOW_HASKELL__ <= 912
import GHC.Driver.Config.Finder
#endif

#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Types.CtLoc
#endif

#if __GLASGOW_HASKELL__ <= 904
type Subst = TCvSubst
#endif

data EffGiven = EffGiven
  { effCon :: Type
  , eff :: Type
  , es :: Type
  }

instance O.Outputable EffGiven where
  ppr given =
    O.text "[G]" O.<+> O.ppr given.eff O.<+> O.text ":>" O.<+> O.ppr given.es

data EffWanted = EffWanted
  { effCon :: Type
  , eff :: Type
  , es :: Type
  , loc :: CtLoc
  }

newtype OtherGiven = OtherGiven
  { ty :: Type
  }

instance O.Outputable OtherGiven where
  ppr given =
    O.text "[G]" O.<+> O.ppr given.ty

instance O.Outputable EffWanted where
  ppr wanted =
    O.text "[W]" O.<+> O.ppr wanted.eff O.<+> O.text ":>" O.<+> O.ppr wanted.es

data OtherWanted = OtherWanted
  { ty :: Type
  , vars :: CoVarSet
  }

instance O.Outputable OtherWanted where
  ppr wanted =
    O.text "[W]" O.<+> O.ppr wanted.ty

data Candidates = None | Single EffGiven | Multiple

----------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just TcPlugin
    { tcPluginInit = initPlugin
    , tcPluginRewrite = \_ -> emptyUFM
    , tcPluginSolve = disambiguateEffects
    , tcPluginStop = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }

initPlugin :: TcPluginM Class
initPlugin = do
  clsMod <- lookupModule $ mkModuleName "Effectful.Internal.Effect"
  cls <- tcLookupClass =<< lookupOrig clsMod (mkTcOcc ":>")
  pure cls
  where
    lookupModule :: ModuleName -> TcPluginM Module
    lookupModule modName = do
      hscEnv <- getTopEnv
      findPluginModuleCompat hscEnv modName >>= \case
        Found _ md -> pure md
        _ -> errorWithoutStackTrace "Please add effectful-core to the list of dependencies."

disambiguateEffects
  :: Class
  -> EvBindsVar
  -> [Ct]
  -> [Ct]
  -> TcPluginM TcPluginSolveResult
disambiguateEffects elemCls _ allGivens allWanteds = do
  printList "EffGivens" effGivens
  printList "OtherGivens" otherGivens
  printList "EffWanteds" effWanteds
  printList "OtherWanteds" otherWanteds
  instEnvs <- getInstEnvs
  solutions <- tcPluginIO $ newIORef []
  forM_ effWanteds $ \wanted -> do
    printSingle "Wanted" wanted
    let extraEffGivens = extractEffGivens wanted.es wanted.es
    case mapMaybe (maybeUnifiesWith wanted) $ extraEffGivens ++ effGivens of
      [] -> printLn "No candidates"
      [(given, _)] -> do
        printSingle "Single candidate found" given
        emitEqConstraint solutions wanted given
      candidates -> do
        printList "Multiple candidates found" $ map fst candidates
        filterCandidates instEnvs None candidates >>= \case
          None -> printLn "No candidates left"
          Single given -> do
            printSingle "Single candidate left" given
            emitEqConstraint solutions wanted given
          Multiple -> printLn "Multiple candidates left"
  printLn ""
  TcPluginSolveResult [] [] <$> tcPluginIO (readIORef solutions)
  where
    (otherGivens, effGivens) = partitionEithers
      . map (groupGivens elemCls)
      . filter (not . isIP)
      $ allGivens

    (otherWanteds, effWanteds) = partitionEithers
      . map (groupWanteds elemCls)
      . filter (not . isIP)
      $ allWanteds

    filterCandidates
      :: InstEnvs
      -> Candidates
      -> [(EffGiven, Subst)]
      -> TcPluginM Candidates
    filterCandidates instEnvs acc = \case
      [] -> pure acc
      ((given, subst) : rest) -> do
        printSingle "Candidate" given
        let relevantWanteds = (`mapMaybe` otherWanteds) $ \wanted ->
              if substHasAnyVar subst wanted.vars
              then Just $ substTy subst wanted.ty
              else Nothing
        printList "Relevant wanteds" relevantWanteds
        allWantedsSolvable relevantWanteds >>= \case
          True -> do
            printLn "Candidate fits"
            case acc of
              None -> filterCandidates instEnvs (Single given) rest
              Single _ -> pure Multiple
              Multiple -> error "unreachable"
          False -> do
            printLn "Candidate doesn't fit, skipping"
            filterCandidates instEnvs acc rest
      where
        allWantedsSolvable :: [Type] -> TcPluginM Bool
        allWantedsSolvable = \case
          [] -> pure True
          wanted : rest -> do
            printSingle "Checking" wanted
            if wanted `unifiesWithAny` otherGivens
              then do
                printLn "Solvable from local context"
                allWantedsSolvable rest
              else case tcSplitTyConApp wanted of
                (con, args) -> case tyConClass_maybe con of
                  Nothing -> do
                    printLn "Not a class constraint"
                    pure False
                  Just cls -> case lookupInstEnv False instEnvs cls args of
                    ([], _, _) -> do
                      printLn "No matching instances found"
                      pure False
                    _ -> do
                      printLn "Found matching instances"
                      allWantedsSolvable rest

----------------------------------------
-- Standalone helpers

findPluginModuleCompat :: HscEnv -> ModuleName -> TcPluginM FindResult
findPluginModuleCompat hsc_env mod_name = do
#if __GLASGOW_HASKELL__ <= 912
  let dflags = hsc_dflags hsc_env
      fopts = initFinderOpts dflags
      fc = hsc_FC hsc_env
      units = hsc_units hsc_env
      home_unit = hsc_home_unit hsc_env
  tcPluginIO (findPluginModule fc fopts units (Just home_unit) mod_name)
#else
  tcPluginIO (findPluginModule hsc_env mod_name)
#endif

-- | Record a wanted equality constraint to aid typechecking.
emitEqConstraint :: IORef [Ct] -> EffWanted -> EffGiven -> TcPluginM ()
emitEqConstraint solutions wanted given = do
  let predTy =
#if __GLASGOW_HASKELL__ <= 912
        mkPrimEqPred wanted.eff given.eff
#else
        mkNomEqPred wanted.eff given.eff
#endif
  printSingle "Emitting constraint" predTy
  ev <- newWanted wanted.loc predTy
  tcPluginIO $ modifyIORef' solutions (mkNonCanonical ev :)

-- | Separate givens based on whether they're of the form @e :> es@ or not.
groupGivens :: Class -> Ct -> Either OtherGiven EffGiven
groupGivens elemCls = \case
#if __GLASGOW_HASKELL__ < 908
  CDictCan
    { cc_class = cls
    , cc_tyargs = [eff, es]
    }
    | cls == elemCls ->
#else
  CDictCan DictCt
    { di_cls = cls
    , di_tys = [eff, es]
    }
    | cls == elemCls ->
#endif
    Right EffGiven
      { effCon = fst $ splitAppTys eff
      , eff = eff
      , es = es
      }
  ct -> Left OtherGiven
    { ty = ctPred ct
    }

-- | Separate wanteds based on whether they're of the form @e :> es@ or not.
groupWanteds :: Class -> Ct -> Either OtherWanted EffWanted
groupWanteds elemCls = \case
#if __GLASGOW_HASKELL__ < 908
  CDictCan
    { cc_ev = CtWanted { ctev_loc = loc }
    , cc_class = cls
    , cc_tyargs = [eff, es]
    }
    | cls == elemCls ->
#elif __GLASGOW_HASKELL__ <= 912
  CDictCan DictCt
    { di_ev = CtWanted { ctev_loc = loc }
    , di_cls = cls
    , di_tys = [eff, es]
    }
    | cls == elemCls ->
#else
  CDictCan DictCt
    { di_ev = CtWanted WantedCt { ctev_loc = loc }
    , di_cls = cls
    , di_tys = [eff, es]
    }
    | cls == elemCls ->
#endif
    Right EffWanted
      { effCon = fst $ splitAppTys eff
      , eff = eff
      , es = es
      , loc = loc
      }
  ct ->
    Left OtherWanted
      { ty = ctPred ct
      , vars = tyCoVarsOfType $ ctPred ct
      }

-- | We don't get appropriate given constraints when dealing with concrete (or
-- partially concrete) effect lists like (A : B : C : es), so they need to be
-- manually added (GHC will conjure them later).
extractEffGivens :: Type -> Type -> [EffGiven]
extractEffGivens fullEs es = case splitAppTys es of
  (_colon, [_kind, eff, esTail]) ->
    let (effCon, _tyArgs) = splitAppTys eff
    in EffGiven { effCon = effCon
                , eff = eff
                , es = fullEs
                } : extractEffGivens fullEs esTail
  _ -> []

-- | Check if a constraint in an implicit parameter. We discard all of them
-- since they will not affect resolution of @:>@ constraints.
isIP :: Ct -> Bool
isIP = \case
#if __GLASGOW_HASKELL__ < 908
  CDictCan { cc_class = cls } -> isIPClass cls
#else
  CDictCan DictCt { di_cls = cls } -> isIPClass cls
#endif
  _ -> False

-- | Attempt to unify types, but skip skolem (rigid) type variables. This is
-- crucial for proper filtering of candidates.
tcUnifyTyNoSkolems :: Type -> Type -> Maybe Subst
tcUnifyTyNoSkolems ty1 ty2 = tcUnifyTys bindFun [ty1] [ty2]
  where
    bindFun var _ty = if isSkolemTyVar var then dontBindMe else BindMe

    dontBindMe =
#if __GLASGOW_HASKELL__ <= 912
      Apart
#else
      DontBindMe
#endif

unifiesWithAny :: Type -> [OtherGiven] -> Bool
unifiesWithAny ty = any (isJust . tcUnifyTyNoSkolems ty . (.ty))

substHasAnyVar :: Subst -> TyCoVarSet -> Bool
substHasAnyVar subst = uniqSetAny (`elemUFM` getTvSubstEnv subst)

-- | Given a wanted constraint and a given constraint, attempt to unify them and
-- give back a substitution that can be applied to the wanted to make it equal
-- to the given.
maybeUnifiesWith :: EffWanted -> EffGiven -> Maybe (EffGiven, Subst)
maybeUnifiesWith wanted given =
  if wanted.es `eqType` given.es && wanted.effCon `eqType` given.effCon
  then (given, ) <$> tcUnifyTyNoSkolems wanted.eff given.eff
  else Nothing

----------------------------------------
-- Debugging

#ifdef VERBOSE

showOut :: O.Outputable o => o -> String
showOut = O.showSDocOneLine O.defaultSDocContext . O.ppr

printSingle :: O.Outputable x => String -> x -> TcPluginM ()
printSingle header x = printLn $ header ++ ": " ++ showOut x

printList :: O.Outputable x => String -> [x] -> TcPluginM ()
printList header = \case
  [] -> printLn $ header ++ ": []"
  xs -> do
    printLn $ header ++ ":"
    forM_ xs $ \x -> printLn $ "- " ++ showOut x

printLn :: String -> TcPluginM ()
printLn = tcPluginIO . putStrLn

#else

printSingle :: String -> x -> TcPluginM ()
printSingle _ _ = pure ()

printList :: String -> [x] -> TcPluginM ()
printList _ _ = pure ()

printLn :: String -> TcPluginM ()
printLn _ = pure ()

#endif
