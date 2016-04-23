--------------------------------------------------------------------------------

-- | This module contains the abstract syntax of Hindley-Milner types.
module Language.HM.Type (
    module Language.HM.Alpha,

    -- * Monomorphic types.
    TauF(..),
    Tau(..),
    varT,
    arrowT,

    -- * Polymorphic types.
    SigmaF(..),
    Sigma(..),
    forAllT,
    monoT,

    HasTypeVars(..)
) where

--------------------------------------------------------------------------------

import Data.Fix
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Language.HM.Alpha

--------------------------------------------------------------------------------

data TauF r
    = VarT String
    | ArrowT r r
    deriving (Eq, Show, Functor)

-- | Monomorphic types.
type Tau = Fix TauF

-- | 'varT' @x@ constructs a type variable named @x@.
varT :: String -> Tau
varT = Fix . VarT

-- | 'arrowT' @t0 t1@ constructs an arrow type from @t0@ to @t1@.
arrowT :: Tau -> Tau -> Tau
arrowT t0 t1 = Fix $ ArrowT t0 t1

--------------------------------------------------------------------------------

data SigmaF r
    = ForAllT String r
    | MonoT Tau
    deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Polymorphic types.
type Sigma = Fix SigmaF

-- | 'forAllT' @x t@ universally quantifies @x@ in @t@.
forAllT :: String -> Sigma -> Sigma
forAllT x t = Fix $ ForAllT x t

-- | 'monoT' @t@ lifts a monomorophic type @t@ to a polymorphic one.
monoT :: Tau -> Sigma
monoT = Fix . MonoT

instance AlphaEq Sigma where
    alphaEq t0 t1 = sigmaEq M.empty (unFix t0) (unFix t1)
        where
            tauEq env (VarT x) (VarT y) = case M.lookup x env of
                -- the variable is bound in the left expression: check that
                -- it matches the name of the variable in the right expression
                -- that was bound at the same point
                Just y' -> y == y'
                -- the variable is free in the left expression: it should have
                -- the same name as the variable in the right expression
                Nothing -> x == y
            tauEq env (ArrowT t0 t1) (ArrowT t0' t1') =
                tauEq env (unFix t0) (unFix t0') &&
                tauEq env (unFix t1) (unFix t1')
            tauEq _ _ _ = False

            sigmaEq env (MonoT t0) (MonoT t1) =
                tauEq env (unFix t0) (unFix t1)
            sigmaEq env (ForAllT x t0) (ForAllT y t1) =
                sigmaEq (M.insert x y env) (unFix t0) (unFix t1)
            sigmaEq _ _ _ = False

--------------------------------------------------------------------------------

-- | The class of types which have free type variables.
class HasTypeVars a where
    -- | 'tyVars' @t@ calculates the set of free type variables in @t@.
    tyVars :: a -> S.Set String

    -- | 'tyVarsInOrder' @t@ is like 'tyVars' @t@, except that the type
    -- variables are returned in the order in which they are encountered.
    tyVarsInOrder :: a -> [String]

instance HasTypeVars Tau where
    tyVars = cata go
        where
            go (VarT x) = S.singleton x
            go (ArrowT l r) = l `S.union` r

    tyVarsInOrder = L.nub . cata go
        where
            go (VarT x) = [x]
            go (ArrowT l r) = l ++ r

instance HasTypeVars Sigma where
    tyVars = cata go
        where
            go (MonoT t) = tyVars t
            go (ForAllT x t) = S.delete x t

    tyVarsInOrder = L.nub . cata go
        where
            go (MonoT t) = tyVarsInOrder t
            go (ForAllT x t) = L.delete x t

--------------------------------------------------------------------------------
