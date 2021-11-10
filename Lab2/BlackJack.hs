module BlackJack where
import System.Random
import Cards
import RunGame
import Test.QuickCheck

-- A0 --

-- test cards "Helen of Troy" and "Randy can b Anything"
helen = Card Queen Hearts
randy = Card Ace Spades

-- a testhand for testing
testHand :: Hand
testHand = Add (helen) (Add (randy) (Add(randy) Empty))

sizeSteps :: [Integer]
sizeSteps = [ size testHand , size (Add (Card (Numeric 2) Hearts) 
 (Add (Card Jack Spades) Empty)), 1 + size 
 (Add (Card Jack Spades) Empty), 1 + 1 + size Empty, 2]

-- A1 --

--Displays a card
displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++ show suit
displayCard (Card rank suit)        = show rank ++ " of " ++ show suit

--Displays a hand of cards
display :: Hand -> String 
display Empty           = ""
display (Add card hand) = displayCard card ++ ", " ++ display hand

-- A2 --

-- Calculates intiger value for all the ranks in the hand
value :: Hand -> Integer
value h = if (intitalValue h) > 21
            then aceProblem h 
            else intitalValue h

-- Value calculated with 11 for ace
intitalValue :: Hand -> Integer
intitalValue Empty = 0
intitalValue (Add (Card (Numeric i) _) h) = i + intitalValue h
intitalValue (Add (Card r _) h)           | r == Ace  = 11 + intitalValue h
                                          | otherwise = 10 + intitalValue h

-- Value calculated with 1 for ace
aceProblem :: Hand -> Integer
aceProblem Empty = 0
aceProblem (Add (Card (Numeric i) _) h) = i + aceProblem h
aceProblem (Add (Card r _) h)           | r == Ace  = 1 + aceProblem h
                                        | otherwise = 10 + aceProblem h

-- A3 --

-- True if hand value exceeds 21
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- A4 --

-- Winning conditions
winner :: Hand -> Hand -> Player
winner guestHand bankHand | gameOver guestHand               = Bank
                          | gameOver bankHand                = Guest
                          | value guestHand > value bankHand = Guest
                          | otherwise                        = Bank


-- B1 --

-- Combines hands, puttning 1 a top of 2
(<+) :: Hand -> Hand -> Hand
Empty         <+ h  = h
(Add card h1) <+ h2 = (Add card (h1 <+ h2))

-- Tests if <+ is associative.
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

-- Tests if the combined hand is the same size as the sum of its part
prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = size (h1 <+ h2) == (size h1) + (size h2)

-- B2 --

-- A list of all the ranks
rankList :: [Rank]
rankList = [Numeric i | i <- [2..10]] ++ [Jack, Queen, King, Ace]

-- A list of all the suits
suitList :: [Suit]
suitList = [Hearts, Spades, Diamonds, Clubs]

-- A list of every possible rank and suit combo.
cardList :: [Card]
cardList = [Card rank suit | rank <- rankList, suit <- suitList]

-- Converts card list to a hand.
cardListToHand ::  Hand -> [Card] -> Hand
cardListToHand  hand []    = hand
cardListToHand hand (c:cs) = (Add c (cardListToHand hand cs))

-- Returns the full deck.
fullDeck :: Hand
fullDeck = cardListToHand Empty cardList

-- B3 --

-- Move one card to another Hand
draw :: Hand -> Hand -> (Hand,Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add c deck) hand = (deck, Add c hand)

-- B4 --

-- Plays for the Bank
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

-- Makes the Bank play until 16+ or fat.
playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand = if value hand < 16 
                             then playBankHelper smallerDeck biggerHand
                             else hand
                         where (smallerDeck, biggerHand) = draw deck hand

-- Returns the old/new deck after the card has been moved
cardMover :: Int -> (Hand, Hand) -> (Hand,Hand)
cardMover 0 ((Add card oldDeck), newDeck) = (oldDeck,(Add card newDeck))
cardMover i ((Add card oldDeck), newDeck) = ((Add card newOldDeck),newNewDeck)
            where (newOldDeck,newNewDeck) = cardMover (i-1) (oldDeck,newDeck)


-- B5 --

-- Will repeat calling cardMover until the oldDeck is Empty
shuffleHelp :: StdGen -> (Hand,Hand) -> Hand
shuffleHelp random (Empty, newDeck) = newDeck
shuffleHelp random (oldDeck, newDeck) = shuffleHelp random2 (cardMover i (oldDeck, newDeck) )
  where (i,random2) =  (randomR (0, size oldDeck -1) random)

-- Shuffles a hand
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck random deck = shuffleHelp random (deck, Empty)

-- Checks if a card belongs to a hand
belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

-- Checks if no card is lost during shuffle.
prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h = c `belongsTo` h == c `belongsTo` shuffleDeck g h

-- Checks if the hand size remains the same after shuffle
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffleDeck g h) 

-- B6 --

implementation = Interface
  { iFullDeck = fullDeck
    , iValue = value
    , iDisplay = display
    , iGameOver = gameOver
    , iWinner = winner
    , iDraw = draw
    , iPlayBank = playBank
    , iShuffle = shuffleDeck
  }

main :: IO ()
main = runGame implementation


