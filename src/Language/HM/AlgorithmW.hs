--------------------------------------------------------------------------------

module Language.HM.AlgorithmW (
    -- * Typing contexts
    Context,
    empty,

    -- * External interface
    runW,
    inferW,

    -- * Misc
    genInOrder
) where

--------------------------------------------------------------------------------

import Data.Fix
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Language.HM.Term
import Language.HM.Theta
import Language.HM.Type
import Language.HM.TypeError

--------------------------------------------------------------------------------

-- | Typing contexts.
type Context = M.Map Var Sigma

-- | 'empty' is the empty typing context.
empty :: Context
empty = M.empty

instance CanApply Context where
    apply s = M.map (apply s)

--------------------------------------------------------------------------------

-- | The type of Algorithm W computations.
newtype W a = W { unW :: Context -> Int -> Either TypeError (a, Int) }

instance Functor W where
    f `fmap` (W m) = W $ \ctx n -> do
        (r,n') <- m ctx n
        return (f r, n')

instance Applicative W where
    pure x = W $ \_ n -> return (x, n)

    (W m) <*> (W m') = W $ \ctx n -> do
        (f, n')  <- m ctx n
        (x, n'') <- m' ctx n'
        return (f x, n'')

instance Monad W where
    W m >>= f = W $ \ctx n -> do
        (r, n') <- m ctx n
        let W m' = f r in m' ctx n'

--------------------------------------------------------------------------------

-- | 'fresh' returns a fresh type variable.
fresh :: W Tau
fresh = W $ \ctx n -> return (varT ('$' : show n), n + 1)

typeError :: TypeError -> W a
typeError err = W $ \_ _ -> Left err

--------------------------------------------------------------------------------

-- | 'gen' @t@ generalises a monomorphic type @t@ to a polymorphic type.
gen :: Tau -> W Sigma
gen t = W $ \ctx n -> return (gen' ctx t, n)

gen' :: Context -> Tau -> Sigma
gen' ctx t = S.foldr forAllT (monoT t) vs
    where
        cs = S.unions $ map tyVars $ M.elems ctx
        vs = tyVars t `S.difference` cs

-- | 'genInOrder' @ctx t@ generalises a monomorphic type @t@ to a polymorphic
-- type in a context @ctx@. This variant of 'gen' ensures that the order of
-- quantifiers matches that in which type variables occur in @t@.
genInOrder :: Context -> Tau -> Sigma
genInOrder ctx t = foldr forAllT (monoT t) vs
    where
        cs = S.unions $ map tyVars $ M.elems ctx
        vs = tyVarsInOrder t L.\\ S.toList cs

-- | 'inst' @t@ instantiates a polymorphic type @t@ with fresh type variables.
inst :: Sigma -> W Tau
inst = cataM go
    where
        go (MonoT t) = return t
        go (ForAllT x t) = do
            i <- fresh

            let s = M.singleton x i

            return (apply s t)

-- | 'withContext' @f m@ runs applies @f@ to the typing context in which @m@ is
-- run. The context of the overall computation is not affected.
withContext :: (Context -> Context) -> W a -> W a
withContext f (W m) = W $ \ctx n -> m (f ctx) n

-- | 'lookupType' @x@ looks up the type of @x@ in the context.
lookupType :: Var -> W (Maybe Sigma)
lookupType x = W $ \ctx n -> return (M.lookup x ctx, n)

-- | 'requireType' @x@ looks up the type of @x@ in the context. A type error
-- is raised if @x@ is not typed in the context.
requireType :: Var -> W Sigma
requireType x = lookupType x >>= \r -> case r of
    Nothing -> typeError $ NotInScopeErr x
    Just t  -> return t

--------------------------------------------------------------------------------

-- | 'bind' @x t@ binds a type @t@ to a type variable named @x@.
bind :: String -> TauF (Fix TauF) -> W Theta
bind x (VarT y) | x == y = return M.empty
bind x t
    | x `S.member` tyVars (Fix t) = typeError $ OccursErr x (Fix t)
    | otherwise = return $ M.singleton x (Fix t)

-- | 'unify' @t0 t1@ unifies two types @t0@ and @t1@. If the two types can be
-- unified, a substitution is returned which unifies them.
unify :: Tau -> Tau -> W Theta
unify t0 t1 = go (unFix t0) (unFix t1)
    where
        go (VarT x) t = bind x t
        go t (VarT x) = bind x t
        go (ArrowT f x) (ArrowT g y) = do
            s0 <- go (unFix f) (unFix g)
            s1 <- go (unFix $ apply s0 x) (unFix $ apply s0 y)
            return (s1 <@> s0)
        -- without base types etc., all types are unifiable
        --  go t0 t1 = typeError $ UnifyErr (Fix t0) (Fix t1)

--------------------------------------------------------------------------------

-- | 'infer' @term@ reconstructs types in @term@.
infer :: Term -> W TyTerm
infer term = (\(s,_,e) -> apply s e) `fmap` cata go term
    where
        go :: TermF Var (W (Theta, Tau, TyTerm)) -> W (Theta, Tau, TyTerm)
        go (Var x) = do
            -- @x@ must be typed in the context
            pt <- requireType x

            -- instantiate the type of @x@
            mt <- inst pt

            -- return an annotated variable along with an empty substituion
            return (M.empty, mt, tyVarE (Typed x (monoT mt)) mt)
        go (App f x) = do
            (s0, t0, tf) <- f
            (s1, t1, tx) <- withContext (apply s0) x

            -- generate a fresh type variable to represent the return type
            -- of the function
            mt <- fresh

            -- unify the types
            s2 <- unify t0 (t1 `arrowT` mt)

            -- return the annotated application
            return (s2 <@> s1 <@> s0, apply s2 mt, tyAppE tf tx mt)
        go (Abs x e) = do
            -- we need a fresh type variable for @x@
            mt <- fresh

            --
            (s0, t0, te) <- withContext (M.insert x (monoT mt)) e

            let rt = arrowT mt t0

            -- return the annotated abstraction
            return (s0, apply s0 rt, tyAbsE (Typed x (monoT mt)) te rt)
        go (Let x e0 e1) = do
            -- infer the type of the expression that is being bound
            (s0, t0, te0) <- e0

            -- generalise the type of the expression that is being bound
            pt <- withContext (apply s0) (gen t0)

            -- infer the type of the other expression
            (s1, t1, te1) <- withContext (apply s0 . M.insert x pt) e1

            -- return the annotated let binding
            return (s1 <@> s0, t1, tyLetE (Typed x pt) te0 te1 t1)

--------------------------------------------------------------------------------

-- | 'runW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns an updated @term@ with explicit type annotations.
runW :: Context -> Term -> Either TypeError TyTerm
runW ctx term = fst `fmap` unW (infer term) ctx 0

-- | 'inferW' @gamma term@ runs Algorithm W on @term@ with an initial context
-- @gamma@ and returns the polymorphic type of the whole term.
inferW :: Context -> Term -> Either TypeError Tau
inferW ctx term = (tyAnn . unTypedF . unFix) `fmap` runW ctx term

--------------------------------------------------------------------------------
