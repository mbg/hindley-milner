--------------------------------------------------------------------------------

module Language.HM.TypeSpec where

--------------------------------------------------------------------------------

import Test.Hspec

import qualified Data.Set as S

import Language.HM

--------------------------------------------------------------------------------


t0 :: Sigma
t0 = forAllT "a" (monoT $ (varT "a") `arrowT` (varT "a"))

t1 :: Sigma
t1 = forAllT "b" (monoT $ (varT "b") `arrowT` (varT "b"))

t2 :: Sigma
t2 = forAllT "b" (monoT $ (varT "b") `arrowT` (varT "a"))

t3 :: Sigma
t3 = monoT $ varT "a"

t4 :: Sigma
t4 = monoT $ varT "b"

spec :: Spec
spec = do
    describe "alpha equivalence" $ do
        context "are alpha equivalent" $ do
            it "same type" $ alphaEq t0 t0 `shouldBe` True
            it "alpha-equivalent type" $ alphaEq t0 t1 `shouldBe` True

        context "are not alpha equivalent" $ do
            it "free variable 1" $ alphaEq t0 t2 `shouldBe` False
            it "free variable 2" $ alphaEq t3 t4 `shouldBe` False

    describe "type variables" $ do
        it "t0" $ tyVars t0 `shouldBe` S.fromList []
        it "t1" $ tyVars t1 `shouldBe` S.fromList []
        it "t2" $ tyVars t2 `shouldBe` S.fromList ["a"]
        it "t3" $ tyVars t3 `shouldBe` S.fromList ["a"]
        it "t4" $ tyVars t4 `shouldBe` S.fromList ["b"]


--------------------------------------------------------------------------------
