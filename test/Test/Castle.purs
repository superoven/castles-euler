module Test.Castle where

import Prelude

import Castle (getCount, isEvenCastle, makeInitialArray, newCastle, pb)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (RunnerEffects)

castleTestSpec :: ∀ e. Spec (RunnerEffects e) Unit
castleTestSpec =
  describe "Castle" do
    it "Makes Initial Array" $
      (makeInitialArray 4 2) `shouldEqual` [[true,true,true,true],[false,false,false,false]]
    it "equality" $
        (pb 1 1 <<< pb 3 1) (newCastle 4 2) `shouldEqual` (pb 3 1 <<< pb 1 1) (newCastle 4 2)
    it "equality out of range" $
        (pb 1 4 <<< pb 3 1) (newCastle 4 2) `shouldEqual` (pb 3 1) (newCastle 4 2)
    describe "Even Castles" do
      it "Simple Failure" $
        isEvenCastle ((pb 1 1 <<< pb 3 1) (newCastle 4 2)) `shouldEqual` false
      it "Simple Success" $
        isEvenCastle (pb 1 1 (newCastle 4 2)) `shouldEqual` true
      it "More complex success" $
        isEvenCastle ((pb 1 1 <<< pb 3 1 <<< pb 2 1) (newCastle 4 3)) `shouldEqual` true
      it "More complex failure" $
        isEvenCastle ((pb 1 1 <<< pb 3 1 <<< pb 2 1 <<< pb 1 2) (newCastle 4 3)) `shouldEqual` false
      it "Add another block" $
        isEvenCastle ((pb 1 1 <<< pb 3 1 <<< pb 2 1 <<< pb 1 2 <<< pb 3 2) (newCastle 4 3)) `shouldEqual` true
    describe "counts" do
      it "counts 4 2" $
        getCount 4 2 `shouldEqual` 10
      it "counts 1 1" $
        getCount 1 1 `shouldEqual` 0
      it "counts 1 2" $
        getCount 1 2 `shouldEqual` 1
      it "counts 1 3" $
        getCount 1 3 `shouldEqual` 1
      it "counts 1 4" $
        getCount 1 4 `shouldEqual` 2
      it "counts 1 5" $
        getCount 1 5 `shouldEqual` 2
      it "counts 2 1" $
        getCount 2 1 `shouldEqual` 0
      it "counts 3 1" $
        getCount 3 1 `shouldEqual` 0
      it "counts 2 2" $
        getCount 2 2 `shouldEqual` 3
      it "counts 2 3" $
        getCount 2 3 `shouldEqual` 3
      it "counts 2 4" $
        getCount 2 4 `shouldEqual` 10
      it "counts 2 5" $
        getCount 2 5 `shouldEqual` 10
      it "counts 3 1" $
        getCount 3 1 `shouldEqual` 0
      it "counts 3 2" $
        getCount 3 2 `shouldEqual` 6
      it "counts 3 3" $
        getCount 3 3 `shouldEqual` 9
      it "counts 3 4" $
        getCount 3 4 `shouldEqual` 40
      it "counts 3 5" $
        getCount 3 5 `shouldEqual` 50
      it "counts 3 6" $
        getCount 3 6 `shouldEqual` 126
      it "counts 3 7" $
        getCount 3 7 `shouldEqual` 147
