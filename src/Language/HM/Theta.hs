--------------------------------------------------------------------------------

-- | Capture-avoiding substitutions.
module Language.HM.Theta where

--------------------------------------------------------------------------------

import Data.Fix
import qualified Data.Map as M

import Language.HM.Type
import Language.HM.Term

--------------------------------------------------------------------------------

-- | Substitutions of type variables for monomorphic types.
type Theta = M.Map String Tau

--------------------------------------------------------------------------------

-- | Class of types to which type substitutions can be applied.
class CanApply a where
    -- | 'apply' @s t@ applies @s@ to some type @t@.
    apply :: Theta -> a -> a

instance CanApply Tau where
    apply s = cata g
        where
            g :: TauF Tau -> Tau
            g (VarT x) = case M.lookup x s of
                Nothing -> varT x
                Just t  -> apply s t
            g (ArrowT t0 t1) = arrowT t0 t1

instance CanApply Sigma where
    apply s = cata g
        where
            g :: SigmaF Sigma -> Sigma
            g (MonoT t) = monoT (apply s t)
            g (ForAllT x t) = forAllT x (apply (M.delete x s) t)

instance CanApply t => CanApply (Typed t a) where
    apply s (Typed x t) = Typed x (apply s t)

instance CanApply TyTerm where
    apply s = cata g
        where
            g (TypedF t) = Fix $ TypedF (apply s t)

-- | @s1@ '<@>' @s2@ applies @s1@ to @s2@.
(<@>) :: Theta -> Theta -> Theta
s1 <@> s2 = M.map (apply s1) s2 `M.union` s1

--------------------------------------------------------------------------------
