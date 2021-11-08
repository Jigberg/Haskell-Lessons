module L01B where
-- Algebraic data types
-- David Sands 
-- 2019-11-07

-- The Suit
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq)
  
--   Concept [c] Show

-- Colour
data Colour = Red | Black
  deriving (Show,Eq)
  
-- colour (Eq)
colour :: Suit -> Colour
colour Spades = Black
colour Clubs  = Black
colour _      = Red

colour' s
    | s == Spades || s == Clubs = Black
    | otherwise                 = Red
         

-- Rank: a datatype for the rank of a card
data Rank =  Numeric Int | Jack | Queen | King | Ace
  deriving (Eq,Show,Ord)
-- more complex types 

-- Datatype invariant 
prop_rank (Numeric n) = n < 11 && n > 1
prop_rank _           = True


-- rank1 `rankBeats` rank2 ?
-- when is one rank higher than another?
-- "longhand" definition in the lecture notes. 

rankBeats r1 r2 =  r1 > r2

-- prop_rankBeats

prop_rankBeats r1 r2 =
   r1 `rankBeats` r2 /= r2 `rankBeats` r1 || r1 == r2
   

-- [c] Finding functions: Hoogle!
-- [c] Don't care patterns
-- [c] Infix syntax (see last lecture ~== function)

-- Card: a data type containing a Rank and a Suit
-- and its projection functions


data Card = Card Rank Suit
  deriving (Show,Eq)
  
rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

-- and shorthand form defining all three
-- data Card = Card {rank::Rank, suit::Suit}

david = Card King Hearts
death = Card Ace Spades


-- cardBeats card1 card2 checks if card1 beats card2
-- w & wo pattern matching
cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) =
              s1 == s2 && r1 `rankBeats` r2

-----------------------------------------------
-- Hand: datatype for a hand of cards
-- data Hand = Hand [Card]

data Hand = Empty | Add Card Hand
   deriving Show

aHand = Add death (Add david Empty)

--handBeats: does one hand beat a given card
-- handBeats :: Hand -> Card -> Bool
handBeats Empty      c = False
handBeats (Add c' h) c = c' `cardBeats` c || h `handBeats` c

-- example: size of a hand
size :: Hand -> Int
size Empty     = 0
size (Add c h) = 1 + size h

length' []     = 0
length' (x:xs) = 1 + length' xs


--- This example is discussed in
-- https://youtu.be/UdgNIuBZWFo

splitHand :: Hand -> (Hand,Hand)
  -- split a deck into the Red and the Black cards

-- Three methods
-- 1. Simple double traversal
splitHand h = (select Red h, select Black h)

select :: Colour -> Hand -> Hand
select col Empty     = Empty
select col (Add c h) | col == colourCard c = Add c (select col h)
                     | otherwise           = select col h
                    
-- 2. Direct recursive definition
splitHand' Empty     = (Empty, Empty)
splitHand' (Add c h)
     | colourCard c == Red = (Add c reds, blacks)
     | otherwise           = (reds, Add c blacks)
    where (reds, blacks) = splitHand' h
    
-- 3. Single recursion using a helper function
--   (using accumulating parameters)

splitHand'' h = split h Empty Empty
  where split Empty     reds blacks = (reds, blacks)
        split (Add c h) reds blacks
          | colourCard c == Red = split h (Add c reds) blacks
          | otherwise           = split h reds         (Add c blacks)


-- Further example ----------

-- See video lecture: https://youtu.be/-L18-DK5y3U?t=1031

-- chooseCard beat hand 
-- chooses a card from hand to play, 
-- when beat is the card to be beaten
   -- If the hand is only one card, play it
   -- If there is a choice,
   --    Select the best card from the rest of the hand
   --    Choose between it and the first card
   -- Principles
   --   1. Follow suit if possible 
   -- (like in "trick winning" games: whist, bridge, ...)
   --   2. Play lowest winning card if possible
   --   3. Play lowest losing card otherwise

-- 
{- 
chooseCard :: Card -> Hand -> Card
chooseCard b (Add c Empty) = c
chooseCard b (Add c h) 
 | tnt c  c' = c
 | tnt c' c  = c'
 | otherwise = rankMin c c'
    where c'        = chooseCard b h
          -- is first card trumps and second non-trumps?
          tnt c1 c2 = suit c1 == suit b 
                    && not (suit c2 == suit b)

rankMin c c' | rank c < rank c' = c 
             | otherwise        = c'

-}

-- Note: A nicer version of this function, broken down into
-- easier pieces, is given in Thomas Hallgren's code from
-- 2018.

-- property: relating chooseCard and handBeats




-------------------------------------------------------
-- The quickCheck "magic" we need to get it to generate arbitrary
-- elements of our new datatypes:
 
{-
instance Arbitrary Suit where
  arbitrary = elements [Spades, Hearts, Diamonds, Clubs]

instance Arbitrary Rank where
  arbitrary =
    oneof $
      [ do return c
      | c <- [Jack,Queen,King,Ace]
      ] ++
      [ do n <- choose (2,10)
           return (Numeric n)
      ]

instance Arbitrary Card where
  arbitrary =
    do r <- arbitrary
       s <- arbitrary
       return (Card r s)

instance Arbitrary Hand where
  arbitrary =
    do cs <- arbitrary
       let hand []     = Empty
           hand (c:cs) = Add c (hand [ c' | c' <- cs, c' /= c ])
       return (hand cs)
  shrink Empty = []
  shrink (Add c h) = h : map (Add c) (shrink h)
-}
-------------------------------------------------------------------------


