module Deck where

import           Card
import           Data.List  (intercalate)
import           Text.Regex

type Deck = [Card]

fullDeck :: Deck
fullDeck = [Card suit rank | suit <- [Clubs .. Spades], rank <- [Two .. Ace]]

split :: Regex
split = mkRegex ",\\s?"

parseDeck :: String -> Deck
parseDeck x = map read $ splitRegex split x

prettyPrintDeck :: Deck -> String
prettyPrintDeck d = intercalate ", " $ map show d
