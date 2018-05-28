module Main where

import           Blackjack
import           Deck
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  deck <-
    if null args
      then shuffle fullDeck
      else readAndParse (head args)
  let (w, g) = play deck
  putStrLn (id (name w))
  print (player g)
  print (dealer g)

readAndParse fileName = do
  text <- readFile fileName
  return $ parseDeck text
