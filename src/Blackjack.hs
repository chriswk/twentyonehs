module Blackjack where

import           Card
import           Debug.Trace (trace)
import           Deck

type Hand = [Card]

data Game = Game
  { player :: Player
  , dealer :: Player
  , deck   :: Deck
  } deriving (Show)

data PlayerType
  = DealerT
  | PlayerT

data Player = Player
  { name         :: String
  , wantsAnother :: Player -> Game -> Bool
  , playerType   :: PlayerType
  , hand         :: Hand
  }

instance Show Player where
  show p = show (name p) ++ ": " ++ (prettyPrintDeck (hand p))

mkPlayer :: String -> Int -> Hand -> Player
mkPlayer name drawLimit = Player name (playerStrategy drawLimit) PlayerT

mkDealer :: Hand -> Player
mkDealer = Player "dealer" dealerStrategy DealerT

playerStrategy :: Int -> Player -> Game -> Bool
playerStrategy hitLimit player game =
  not (hasBlackjack player) &&
  not (hasBlackjack (dealer game)) &&
  not (isBust player) && playerScore player <= hitLimit

dealerStrategy :: Player -> Game -> Bool
dealerStrategy dealer game =
  not (isBust dealer) &&
  not (hasBlackjack dealer) && playerScore dealer <= playerScore (player game)

isBust :: Player -> Bool
isBust p = playerScore p > 21

hasBlackjack :: Player -> Bool
hasBlackjack p = isBlackjack $ hand p

isBlackjack :: Hand -> Bool
isBlackjack hand = length hand == 2 && scoreHand hand == 21

score :: Card -> Int
score c = scoreRank $ rank c

scoreHand :: Hand -> Int
scoreHand hand = sum $ map score hand

playerScore :: Player -> Int
playerScore p = scoreHand $ hand p

scoreRank :: Rank -> Int
scoreRank Two   = 2
scoreRank Three = 3
scoreRank Four  = 4
scoreRank Five  = 5
scoreRank Six   = 6
scoreRank Seven = 7
scoreRank Eight = 8
scoreRank Nine  = 9
scoreRank Ace   = 11
scoreRank _     = 10

initialDeal :: Game -> Game
initialDeal game =
  let (s1:d1:s2:d2:xs) = deck game
      sam' = (player game) {hand = [s1, s2]}
      dealer' = (dealer game) {hand = [d1, d2]}
      deck' = xs
  in game {player = sam', dealer = dealer', deck = deck'}

drawCard :: Deck -> Player -> (Player, Deck)
drawCard [] p = (p, [])
drawCard (x:xs) p =
  let h' = x : (hand p)
  in (p {hand = h'}, xs)

drawUntilDone :: Game -> Player -> Game
drawUntilDone game player =
  let hitMe = wantsAnother player player game
      game' =
        if hitMe
          then let (player', deck') = drawCard (deck game) player
                   g' =
                     case (playerType player') of
                       DealerT -> game {dealer = player', deck = deck'}
                       PlayerT -> game {player = player', deck = deck'}
               in drawUntilDone g' player'
          else game
  in game'

play :: Game -> (Player, Game)
play game =
  let firstPhase = initialDeal game
      afterPlayer =
        trace
          ("Phase is: " ++ show firstPhase)
          drawUntilDone
          firstPhase
          (player firstPhase)
      afterDealer =
        trace
          ("Player has drawn: " ++ show afterPlayer)
          drawUntilDone
          afterPlayer
          (dealer afterPlayer)
  in trace ("Dealer has drawn: " ++ show afterDealer) decideWinner afterDealer

decideWinner :: Game -> (Player, Game)
decideWinner game =
  let p = player game
      d = dealer game
  in if isBust p
       then (d, game)
       else if isBust d
              then (p, game)
              else if hasBlackjack p
                     then (p, game)
                     else if hasBlackjack d
                            then (d, game)
                            else (bestScore p d, game)

bestScore :: Player -> Player -> Player
bestScore p p2 =
  if playerScore p > playerScore p2
    then p
    else p2
