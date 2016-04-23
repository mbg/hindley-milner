--------------------------------------------------------------------------------

-- | This module contains the abstract syntax tree of the term language.
module Language.HM.Term where

--------------------------------------------------------------------------------

import Data.Fix

import Language.HM.Type

--------------------------------------------------------------------------------

-- | The type of variable names.
type Var = String

data TermF v r
    = Var v                 -- ^ Variables.
    | App r r               -- ^ Applications.
    | Abs v r               -- ^ Abstractions.
    | Let v r r             -- ^ Let bindings.
    deriving (Functor, Foldable, Traversable)

-- | The type of terms.
type Term = Fix (TermF Var)

-- | 'varE' @x@ constructs a variable whose name is @x@.
varE :: Var -> Term
varE = Fix . Var

-- | 'appE' @l r@ constructs an application of @l@ to @r@.
appE :: Term -> Term -> Term
appE l r = Fix $ App l r

-- | 'absE' @x e@ constructs an abstraction of @x@ over @e@.
absE :: Var -> Term -> Term
absE x e = Fix $ Abs x e

-- | 'letE' @x e0 e1@ constructs a binding of @e0@ to @x@ in @e1@.
letE :: Var -> Term -> Term -> Term
letE x e0 e1 = Fix $ Let x e0 e1

--------------------------------------------------------------------------------

-- | Things with type annotations.
data Typed a t
    = Typed { untype :: a, tyAnn :: t }

-- | Typed term variables.
type TyVar = Typed Var Sigma

newtype TypedF f t r = TypedF { unTypedF :: Typed (f r) t }

-- | Typed terms.
type TyTerm = Fix (TypedF (TermF TyVar) Tau)

-- | 'tyVarE' @x t@ constructs a variable whose name is @x@ and whose type is
-- @t@.
tyVarE :: TyVar -> Tau -> TyTerm
tyVarE x t = Fix $ TypedF $ Typed (Var x) t

-- | 'tyAppE' @l r t@ constructs an application of @l@ to @r@ whose resulting
-- type is @t@.
tyAppE :: TyTerm -> TyTerm -> Tau -> TyTerm
tyAppE l r t = Fix $ TypedF $ Typed (App l r) t

-- | 'tyAbsE' @x e t@ constructs an abstraction of @x@ over @t@ whose type
-- is @t@.
tyAbsE :: TyVar -> TyTerm -> Tau -> TyTerm
tyAbsE x e t = Fix $ TypedF $ Typed (Abs x e) t

-- | 'tyLetE' @x e0 e1 t@ constructs a binding of @e0@ to @x@ in @e1@ whose
-- resulting type is @t@.
tyLetE :: TyVar -> TyTerm -> TyTerm -> Tau -> TyTerm
tyLetE x e0 e1 t = Fix $ TypedF $ Typed (Let x e0 e1) t

--------------------------------------------------------------------------------
