module Card where

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Bounded)

instance Show Suit where
  show Clubs    = "C"
  show Diamonds = "D"
  show Hearts   = "H"
  show Spades   = "S"

instance Read Suit where
  readsPrec _ input = readsSuit input

readsSuit ('C':s) = [(Clubs, s)]
readsSuit ('D':s) = [(Diamonds, s)]
readsSuit ('H':s) = [(Hearts, s)]
readsSuit ('S':s) = [(Spades, s)]

instance Show Rank where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Read Rank where
  readsPrec _ input = readsRank input

readsRank ('2':s)     = [(Two, s)]
readsRank ('3':s)     = [(Three, s)]
readsRank ('4':s)     = [(Four, s)]
readsRank ('5':s)     = [(Five, s)]
readsRank ('6':s)     = [(Six, s)]
readsRank ('7':s)     = [(Seven, s)]
readsRank ('8':s)     = [(Eight, s)]
readsRank ('9':s)     = [(Nine, s)]
readsRank ('1':'0':s) = [(Ten, s)]
readsRank ('J':s)     = [(Jack, s)]
readsRank ('Q':s)     = [(Queen, s)]
readsRank ('K':s)     = [(King, s)]
readsRank ('A':s)     = [(Ace, s)]


data Card = Card { suit :: Suit,
                   rank :: Rank
                 } deriving (Eq)

instance Show Card where
  show card = show (suit card) ++ show (rank card)

instance Read Card where
  readsPrec _ input = readsCard input

readsCard (s:'1':'0':rest) = [(Card (read [s]) Ten, rest)]
readsCard (s:r:rest)       = [(Card (read [s]) (read [r]), rest)]
