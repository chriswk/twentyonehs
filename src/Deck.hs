module Deck where

import           Card
import           Control.Monad
import           Data.List
import           System.Random
import           Text.Regex

type Deck = [Card]

fullDeck :: Deck
fullDeck = [Card suit rank | suit <- [Clubs .. Spades], rank <- [Two .. Ace]]

splitPattern :: Regex
splitPattern = mkRegex ",\\s?"

parseDeck :: String -> Deck
parseDeck x = map read $ splitRegex splitPattern x

prettyPrintDeck :: Deck -> String
prettyPrintDeck d = intercalate ", " $ map show d

mkRands = mapM (randomRIO . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l =
  let (a, b) = splitAt i l
  in a ++ c : (drop 1 b)

swapElems :: (Int, Int) -> [a] -> [a]
swapElems (i, j) xs
  | i == j = xs
  | otherwise = replaceAt j (xs !! i) $ replaceAt i (xs !! j) xs

shuffle :: [a] -> IO [a]
shuffle xs = liftM (foldr swapElems xs . zip [1 ..]) (mkRands (length xs))
