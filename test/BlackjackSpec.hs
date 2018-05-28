module BlackjackSpec where

import           Blackjack
import           Card
import           Deck

import           Test.Hspec

spec =
  describe "Blackjack" $ do
    it "scores a card correctly" $ do
      score (Card Hearts Eight) `shouldBe` 8
      score (Card Hearts King) `shouldBe` 10
      score (Card Spades Ace) `shouldBe` 11
    it "does not care about suit when scoring" $ do
      score (Card Hearts Seven) `shouldBe` score (Card Spades Seven)
    describe "A player" $ do
        it "wants cards while less than draw limit" $ do
            let player = mkPlayer "sam" 17 
                h = [Card Spades Ace, Card Hearts Five]
                p = player  h
                g = mkEmptyGame
                in (wantsAnother p p g) `shouldBe` True
        it "does not want cards if bust" $ do
            let player = mkPlayer "sam" 25
                h = [Card Spades Ace, Card Hearts Five, Card Hearts Seven]
                p = player h
                g = mkEmptyGame
                in (wantsAnother p p g) `shouldBe` False
        it "does not want cards if it has blackjack" $ do
            let 
                player = mkPlayer "sam" 25
                h = [Card Spades Ace, Card Hearts King]
                p = player h
                g = mkEmptyGame
                in (wantsAnother p) p g `shouldBe` False
    describe "A dealer" $ do
        it "wants cards while less than players" $ do
            let
                g = mkGameWithPlayer
                h = [Card Spades Ace, Card Hearts Seven]
                g' = g { dealer = (dealer g) { hand = h} }
                d = dealer g'
                in (wantsAnother d d g') `shouldBe` True
    describe "Intial deal" $ do
        it "should hand out two cards to each player, alternating players" $ do
            let
                initialDeck = parseDeck "HA, SA, HK, SK"
                g = Game defSam defDealer initialDeck
                g' = initialDeal g
                sam' = player g'
                dealer' = dealer g'
                d' = deck g'
                expectedSamHand = [Card Hearts Ace, Card Hearts King]
                expectedDealerHand = [Card Spades Ace, Card Spades King]
                in
                (hand sam') `shouldBe` expectedSamHand
    describe "Players drawUntilDone" $ do
        it "draws until over limit" $ do
            let
                initialDeck = parseDeck "H2,S2,S3,S4,S5,H4,HK,HQ"
                game = mkEmptyGame { deck = initialDeck }
                drawn = drawUntilDone game (player game)
                sam' = player drawn
                in
                (hand sam') `shouldBe` (reverse [Card Hearts Two, Card Spades Two, Card Spades Three, Card Spades Four, Card Spades Five, Card Hearts Four])
        it "does not keep drawing when one player has blackjack" $ do
            let
                initialDeck = parseDeck "HA,HK,S5"
                game = mkEmptyGame { deck = initialDeck }
                drawn = drawUntilDone game (player game)
                sam' = player drawn
                in
                    (reverse (hand sam')) `shouldBe` [Card Hearts Ace, Card Hearts King]
    describe "Select winner" $ do
        it "Sam wins when both players start with blackjack" $ do
            let
                initialDeck = parseDeck "HA,SA,HK,SK"
                game = mkEmptyGame { deck = initialDeck }
                (w, g) = play game
                in
                (name w) `shouldBe` "sam"
        it "Dealer wins when both players start bust" $ do
            let
                initialDeck = parseDeck "HA,SA,CA,DA"
                game = mkEmptyGame { deck = initialDeck }
                (w, g) = play game
                in
                    (name w) `shouldBe` "dealer"
        it "Sam always wins if he has 21" $ do
            let
                initialDeck = parseDeck "HA,SK,H5,HK,S5,C5"
                game = mkEmptyGame { deck = initialDeck }
                (w, g) = play game
                in
                (name w) `shouldBe` "sam"
        it "Dealer wins when he can beat sam within 21" $ do
            let
                d = parseDeck "SK,HK,H8,C2,C3,C5"
                game = mkEmptyGame { deck = d }
                (w, g) = play game
                in
                    (name w) `shouldBe` "dealer"

defSam :: Player 
defSam = mkPlayer "sam" 17 []

defDealer :: Player 
defDealer = mkDealer []
                    
mkEmptyGame :: Game
mkEmptyGame = Game (mkPlayer "sam" 17 []) (mkDealer []) fullDeck

mkGameWithPlayer :: Game
mkGameWithPlayer =
    let
        p = mkPlayer "sam" 17 nineteen
    in
        Game p (mkDealer []) fullDeck

nineteen :: Hand
nineteen = [Card Spades Ace, Card Hearts Eight]