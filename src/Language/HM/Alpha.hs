--------------------------------------------------------------------------------

-- | Alpha equivalence.
module Language.HM.Alpha where

--------------------------------------------------------------------------------

import qualified Data.Map as M

--------------------------------------------------------------------------------

-- | Class of types which have a notion of alpha equivalence.
class AlphaEq a where
    -- | 'alphaEq' @x y@ determines whether @x@ and @y@ are alpha-equivalent.
    alphaEq :: a -> a -> Bool

--------------------------------------------------------------------------------
