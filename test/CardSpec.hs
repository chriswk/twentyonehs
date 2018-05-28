module CardSpec where

import           Card

import           Control.Applicative
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Suit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Rank where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

spec = describe "Card" $ do
    describe "Suit" $ do
      it "Shows suits with expected output" $ do
        show Clubs `shouldBe` "C"
        show Spades `shouldBe` "S"
        show Diamonds `shouldBe` "D"
        show Hearts `shouldBe` "H"
      it "Read is inverse to show" $ property $
        \x -> (read . show) x == (x :: Suit)

    describe "Rank" $ do
      it "Shows ranks with expected output" $ do
        show Two `shouldBe` "2"
        show Three `shouldBe` "3"
        show Four `shouldBe` "4"
        show Five `shouldBe` "5"
        show Six `shouldBe` "6"
        show Seven `shouldBe` "7"
        show Eight `shouldBe` "8"
        show Nine `shouldBe` "9"
        show Ten `shouldBe` "10"
        show Jack `shouldBe` "J"
        show Queen `shouldBe` "Q"
        show King `shouldBe` "K"
        show Ace `shouldBe` "A"
      it "Read is inverse to show" $ property $
        \x -> (read . show) x == (x :: Rank)

    it "Can combine suit and rank to print a card" $ do
      show (Card Clubs Ace) `shouldBe` "CA"
      show (Card Spades King) `shouldBe` "SK"
      show (Card Hearts Jack) `shouldBe` "HJ"
    it "Read is inverse to show" $ property $
      \x -> (read . show) x == (x :: Card)
