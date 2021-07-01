module FizzBuzzSpec where

import FizzBuzz

-- hspec
import Test.Hspec (Spec, describe, it, shouldBe)

-- QuickCheck
import Test.QuickCheck (arbitrary, forAll, suchThat)

spec :: Spec
spec =
  describe "FizzBuzz" $ do
    it "translate every multiple of 15 to FizzBuzz" $
      forAll arbitrary $
        \i -> fizzBuzzGen config (15 * i) `shouldBe` "FizzBuzz"

    it "translates every multiple of 3 to something starting with Fizz" $
      forAll (fmap (* 3) arbitrary) $
        \i -> take 4 (fizzBuzzGen config i) `shouldBe` "Fizz"

    it "translates every multiple of 5 but not of 3 to Buzz" $
      forAll (arbitrary `suchThat` (\i -> mod i 5 == 0 && mod i 3 /= 0)) $
        \i -> fizzBuzzGen config i `shouldBe` "Buzz"