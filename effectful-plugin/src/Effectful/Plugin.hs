{-# LANGUAGE CPP #-}
module Effectful.Plugin (plugin) where

import Data.Either
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable

import GHC.Core.Class (Class)
import GHC.Core.InstEnv (InstEnvs, lookupInstEnv)
import GHC.Core.Predicate (isIPClass)
import GHC.Core.TyCo.Rep (PredType, Type)
import GHC.Core.TyCo.Subst
import GHC.Core.TyCon (tyConClass_maybe)
import GHC.Core.Type (splitAppTys)
import GHC.Core.Unify (tcUnifyTy)
import GHC.Driver.Config.Finder (initFinderOpts)
import GHC.Driver.Env (hsc_home_unit, hsc_units)
import GHC.Driver.Env.Types (HscEnv (..))
import GHC.Driver.Plugins (Plugin (..), defaultPlugin, purePlugin)
import GHC.Tc.Plugin (getTopEnv, lookupOrig, tcLookupClass, tcPluginIO)
import GHC.Tc.Solver.Monad (newWantedEq, runTcSEarlyAbort)
import GHC.Tc.Types
  ( TcPlugin (..)
  , TcPluginM
  , TcPluginSolveResult (..)
  , unsafeTcPluginTcM
  )
import GHC.Tc.Types.Constraint
  ( Ct (..)
  , CtEvidence (..)
#if __GLASGOW_HASKELL__ < 912
  , CtLoc
#endif
#if __GLASGOW_HASKELL__ >= 908
  , DictCt (..)
#endif
  , ctPred
  , emptyRewriterSet
  )
#if __GLASGOW_HASKELL__ >= 912
import GHC.Tc.Types.CtLoc (CtLoc)
#endif
import GHC.Tc.Types.Evidence (EvBindsVar, Role (..))
import GHC.Tc.Utils.Env (tcGetInstEnvs)
import GHC.Tc.Utils.TcType (tcSplitTyConApp, eqType, nonDetCmpType)
import GHC.Types.Name (mkTcOcc)
import GHC.Types.Unique.FM (emptyUFM)
import GHC.Unit.Finder (FindResult (..), findPluginModule)
import GHC.Unit.Module (Module, ModuleName, mkModuleName)
import GHC.Utils.Outputable (Outputable (..), showSDocUnsafe)

#if __GLASGOW_HASKELL__ >= 906
type TCvSubst = Subst
#endif

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \_ -> Just TcPlugin
    { tcPluginInit = initPlugin
    , tcPluginRewrite = \_ -> emptyUFM
    , tcPluginSolve = solveFakedep
    , tcPluginStop = \_ -> pure ()
    }
  , pluginRecompile = purePlugin
  }

----------------------------------------

data EffGiven = EffGiven
  { givenEffHead :: Type
  , givenEff :: Type
  , givenEs :: Type
  }

instance Show EffGiven where
  show (EffGiven _ e es) =
    "[G] " ++ showSDocUnsafe (ppr e) <> " :> " <> showSDocUnsafe (ppr es)

data EffWanted = EffWanted
  { wantedEffHead :: Type
  , wantedEff :: Type
  , wantedEs :: Type
  , wantedLoc :: CtLoc
  }

instance Show EffWanted where
  show (EffWanted _ e es _) =
    "[W] " <> showSDocUnsafe (ppr e) <> " :> " <> showSDocUnsafe (ppr es)

newtype OrdType = OrdType {unOrdType :: Type}

instance Eq OrdType where
  (==) = eqType `on` unOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` unOrdType

----------------------------------------

type VisitedSet = Set (OrdType, OrdType)

initPlugin :: TcPluginM (Class, IORef VisitedSet)
initPlugin = do
  recMod <- lookupModule $ mkModuleName "Effectful.Internal.Effect"
  cls <- tcLookupClass =<< lookupOrig recMod (mkTcOcc ":>")
  visited <- tcPluginIO $ newIORef Set.empty
  pure (cls, visited)
  where
    lookupModule :: ModuleName -> TcPluginM Module
    lookupModule mod_nm = do
      hsc_env <- getTopEnv
      let dflags = hsc_dflags hsc_env
          fopts = initFinderOpts dflags
          fc = hsc_FC hsc_env
          units = hsc_units hsc_env
          home_unit = hsc_home_unit hsc_env
      tcPluginIO (findPluginModule fc fopts units (Just home_unit) mod_nm) >>= \case
        Found _ md -> pure md
        _ -> errorWithoutStackTrace "Please add effectful-core to the list of dependencies."

solveFakedep
  :: (Class, IORef VisitedSet)
  -> EvBindsVar
  -> [Ct]
  -> [Ct]
  -> TcPluginM TcPluginSolveResult
solveFakedep (elemCls, visitedRef) _ allGivens allWanteds = do
  -- We're given two lists of constraints here:
  --
  -- - 'allGivens' are constraints already in our context,
  --
  -- - 'allWanteds' are constraints that need to be solved.
  --
  -- In the following notes, the words "give/given" and "want/wanted" all refer
  -- to this specific technical concept: given constraints are those that we can
  -- use, and wanted constraints are those that we need to solve.

  --tcPluginIO $ do
  --  putStrLn $ "Givens: " <> show (showSDocUnsafe . ppr <$> allGivens)
  --  putStrLn $ "Wanteds: " <> show (showSDocUnsafe . ppr <$> allWanteds)

  -- For each 'e :> es' we /want/ to solve (the "goal"), we need to eventually
  -- correspond it to another unique /given/ 'e :> es' that will make the
  -- program typecheck (the "solution").
  globals <- unsafeTcPluginTcM tcGetInstEnvs
  let solns = mapMaybe (solve globals) effWanteds

  -- Now we need to tell GHC the solutions. The way we do this is to generate a
  -- new equality constraint, like 'State e ~ State Int', so that GHC's
  -- constraint solver will know that 'e' must be 'Int'.
  eqns <- for solns $ \(goal, soln) -> do
    let wantedEq = newWantedEq (wantedLoc goal) emptyRewriterSet Nominal
                               (wantedEff goal) (givenEff soln)
    (eqn, _) <- unsafeTcPluginTcM $ runTcSEarlyAbort wantedEq
    pure (CNonCanonical eqn, (OrdType $ wantedEff goal, OrdType $ givenEff soln))

  -- For any solution we've generated, we need to be careful not to generate it
  -- again, or we might end up generating infinitely many solutions. So, we
  -- record any already generated solution in a set.
  visitedSolnPairs <- tcPluginIO $ readIORef visitedRef
  let solnEqns = fmap fst . flip filter eqns $ \(_, pair) -> Set.notMember pair visitedSolnPairs
  tcPluginIO $ do
    modifyIORef visitedRef (Set.union $ Set.fromList $ map snd eqns)
    --putStrLn $ "Emitting: " <> showSDocUnsafe (ppr solnEqns)

  pure $ TcPluginSolveResult [] [] solnEqns
  where
    -- The only type of constraint we're interested in solving are 'e :> es'
    -- constraints. Therefore, we extract these constraints out of the
    -- 'allGivens' and 'allWanted's.
    effGivens = mapMaybe maybeEffGiven allGivens
    (otherWantedTys, effWanteds) = partitionEithers
      . map splitWanteds
      -- Get rid of implicit parameters, they're weird.
      . filter (not . isIP)
      $ allWanteds

    -- We store a list of the types of all given constraints, which will be
    -- useful later.
    allGivenTys = ctPred <$> allGivens

    -- Determine if there is a unique solution to a goal from a set of
    -- candidates.
    solve
      :: InstEnvs
      -> EffWanted
      -> Maybe (EffWanted, EffGiven)
    solve globals goal = case unifiableCands of
      -- If there's already only one unique solution, commit to it; in the worst
      -- case where it doesn't actually match, we get a cleaner error message
      -- like "Unable to match (State String) to (State Int)" instead of a type
      -- ambiguity error.
      [(soln, _)] -> Just (goal, soln)
      _ ->
        -- Otherwise, the second criteria comes in: the candidate must satisfy
        -- all other constraints we /want/ to solve. For example, when we want
        -- to solve '(State a :> es, Num a)`, the candidate 'State Int :> es'
        -- will do the job, because it satisfied 'Num a'; however 'State String
        -- :> es' will be excluded.
        let satisfiableCands = filter (satisfiable globals) unifiableCands
        in -- Finally, if there is a unique candidate remaining, we use it as
           -- the solution; otherwise we don't solve anything.
           case satisfiableCands of
             [(soln, _)] -> Just (goal, soln)
             _ -> Nothing
      where
        -- Apart from ':>' constraints in the context, the effects already
        -- hardwired into the effect stack type, like those in 'A : B : C : es'
        -- also need to be considered. So here we extract that for them to be
        -- considered simultaneously with regular ':>' constraints.
        cands = extractExtraGivens (wantedEs goal) (wantedEs goal) <> effGivens
        -- The first criteria is that the candidate constraint must /unify/ with
        -- the goal. This means that the type variables in the goal can be
        -- instantiated in a way so that the goal becomes equal to the
        -- candidate. For example, the candidates 'State Int :> es' and 'State
        -- String :> es' both unify with the goal 'State s :> es'.
        unifiableCands = mapMaybe (unifiableWith goal) cands

    -- Extract the heads of a type like 'A : B : C : es' into 'FakedepGiven's.
    extractExtraGivens :: Type -> Type -> [EffGiven]
    extractExtraGivens fullEs es = case splitAppTys es of
      (_colon, [_kind, e, es']) ->
        let (dtHead, _tyArgs) = splitAppTys e
        in EffGiven { givenEffHead = dtHead
                    , givenEff = e
                    , givenEs = fullEs
                    } : extractExtraGivens fullEs es'
      _ -> []

    -- Determine whether a given constraint is of form 'e :> es'.
    maybeEffGiven :: Ct -> Maybe EffGiven
    maybeEffGiven = \case
#if __GLASGOW_HASKELL__ < 908
      CDictCan { cc_class = cls
               , cc_tyargs = [eff, es]
               } ->
#else
      CDictCan DictCt { di_cls = cls
                      , di_tys = [eff, es]
                      } ->
#endif
        if cls == elemCls
        then Just EffGiven { givenEffHead = fst $ splitAppTys eff
                           , givenEff = eff
                           , givenEs = es
                           }
        else Nothing
      _ -> Nothing

    -- Check if a constraint in an implicit parameter.
    isIP :: Ct -> Bool
    isIP = \case
#if __GLASGOW_HASKELL__ < 908
      CDictCan { cc_class = cls } -> isIPClass cls
#else
      CDictCan DictCt { di_cls = cls } -> isIPClass cls
#endif
      _ -> False

    -- Determine whether a wanted constraint is of form 'e :> es'.
    splitWanteds :: Ct -> Either PredType EffWanted
    splitWanteds = \case
#if __GLASGOW_HASKELL__ < 908
      ct@CDictCan { cc_ev = CtWanted { ctev_loc = loc }
               , cc_class = cls
               , cc_tyargs = [eff, es]
               } ->
#else
      ct@(CDictCan DictCt { di_ev = CtWanted { ctev_loc = loc }
                          , di_cls = cls
                          , di_tys = [eff, es]
                          }) ->
#endif
        if cls == elemCls
        then Right EffWanted { wantedEffHead = fst $ splitAppTys eff
                             , wantedEff = eff
                             , wantedEs = es
                             , wantedLoc = loc
                             }
        else Left $ ctPred ct
      ct -> Left $ ctPred ct

    -- Given a wanted constraint and a given constraint, unify them and give
    -- back a substitution that can be applied to the wanted to make it equal to
    -- the given.
    unifiableWith :: EffWanted -> EffGiven -> Maybe (EffGiven, TCvSubst)
    unifiableWith goal cand =
      if    wantedEs      goal `eqType` givenEs      cand
         && wantedEffHead goal `eqType` givenEffHead cand
      then (cand, ) <$> tcUnifyTy (wantedEff goal) (givenEff cand)
      else Nothing

    -- Check whether a candidate can satisfy all the wanted constraints.
    satisfiable :: InstEnvs -> (EffGiven, TCvSubst) -> Bool
    satisfiable globals (_, subst) = flip all wantedsInst $ \wanted ->
      if Set.member (OrdType wanted) givensInst
        then True -- Can we find this constraint in our local context?
        else case tcSplitTyConApp wanted of
          (con, args) ->
            -- If not, lookup the global environment.
            case tyConClass_maybe con of
              Nothing -> False
              Just cls ->
                let (res, _, _) = lookupInstEnv False globals cls args
                in not $ null res
      where
        -- The wanteds after unification.
        wantedsInst = substTys subst otherWantedTys
        -- The local given context after unification.
        givensInst = Set.fromList (OrdType <$> substTys subst allGivenTys)
