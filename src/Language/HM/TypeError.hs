--------------------------------------------------------------------------------

-- | This module contains types for structured type errors.
module Language.HM.TypeError where

--------------------------------------------------------------------------------

import Language.HM.Type

--------------------------------------------------------------------------------

-- | Type errors.
data TypeError
    = OccursErr String Tau
    | UnifyErr Tau Tau
    | NotInScopeErr String
    deriving (Eq, Show)

--------------------------------------------------------------------------------
