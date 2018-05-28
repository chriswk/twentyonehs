module DeckSpec where

import           Card
import           Deck
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary Suit where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Rank where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Card where
  arbitrary = Card <$> arbitrary <*> arbitrary

spec =
  describe "Deck" $ do
    it "consists of 52 cards" $ do length fullDeck `shouldBe` 52
    it "can parse a string into multiple cards" $ do
      parseDeck "HA, HK, SK, SQ" `shouldBe`
        [Card Hearts Ace, Card Hearts King, Card Spades King, Card Spades Queen]
    it "handles deck strings with and without space in seperator" $ do
      parseDeck "HA, HK, SK, SQ" `shouldBe` parseDeck "HA,HK,SK,SQ"
    it "can parse output of all decks back to deck" $
      property $ \d -> (parseDeck . prettyPrintDeck) d == (d :: Deck)
