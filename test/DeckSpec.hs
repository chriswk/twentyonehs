module DeckSpec where

import           Card
import           Deck
import           Test.Hspec

spec = describe "Deck" $ do
  it "consists of 52 cards" $ do
    length deck `shouldBe` 52
  it "can parse a string into multiple cards" $ do
    parseDeck "HA, HK, SK, SQ" shouldBe [Card Hearts Ace, Card Hearts King, Card Spades King, Card Spades Queen]
