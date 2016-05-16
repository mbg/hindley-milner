--------------------------------------------------------------------------------

module Language.HM.AlgorithmWSpec where

--------------------------------------------------------------------------------

import Test.Hspec

import Language.HM
import Language.HM.AlgorithmW

--------------------------------------------------------------------------------

e0 :: Term
e0 = varE "x"

e1 :: Term
e1 = letE "id" (absE "x" (varE "x")) (varE "id")

t1 :: Tau
t1 = varT "a" `arrowT` varT "a"

e2 :: Term
e2 = letE "id"
        (absE "x" (varE "x"))
        (appE (varE "id") (varE "id"))

e3 :: Term
e3 = letE "id"
        (absE "x" (letE "y" (varE "x") (varE "y")))
        (appE (varE "id") (varE "id"))

e4 :: Term
e4 = letE "id"
        (absE "x" (appE (varE "x") (varE "x")))
        (varE "id")

-- | 'alphaEqOf' @t r@ is a predicate which tests that the result of some
-- type inference process @r@ is successful and alpha-equivalent to @t@.
alphaEqOf :: Tau -> Either TypeError Tau -> Bool
alphaEqOf _ (Left _)   = False
alphaEqOf t (Right t') = alphaEq (genInOrder empty t) (genInOrder empty t')

occursFailure :: Either TypeError Tau -> Bool
occursFailure (Left (OccursErr _ _)) = True
occursFailure _ = False

spec :: Spec
spec = do
    it "not in scope" $ inferW empty e0 `shouldBe` Left (NotInScopeErr "x")
    it "identity function 1" $ inferW empty e1 `shouldSatisfy` alphaEqOf t1
    it "identity function 2" $ inferW empty e2 `shouldSatisfy` alphaEqOf t1
    it "identity function 3" $ inferW empty e3 `shouldSatisfy` alphaEqOf t1
    it "identity function 4" $ inferW empty e4 `shouldSatisfy` occursFailure

--------------------------------------------------------------------------------
