module Deck where

import           Card

type Deck = [Card]

deck :: Deck
deck = [Card suit rank | suit <- [Clubs .. Spades], rank <- [Two .. Ace]]
