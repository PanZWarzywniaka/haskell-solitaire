{-
    Author: Aleksander Osikowicz
    Title: Grading Assignment 2108 Haskell Solitaire
    Last Modification: 08/12/2021
-}

module Solitaire where
import System.Random
import Data.List
import Data.Foldable
import Data.Maybe
import Data.Function
import Control.Concurrent

-- basic type definitions
data Suit =  Hearts | Clubs | Spades | Diamonds
    deriving (Show,Enum,Eq) --using enum class enables creating list of all Suits

data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show,Enum,Eq) --using enum class enables creating list of all Pips


--Card definition

data Card = Card Pip Suit Bool --Bool indicates if card is facing up or down, True if facing up
instance Eq Card where
    (==) (Card pip1 suit1 _) (Card pip2 suit2 _) = pip1 == pip2 && suit1 == suit2

instance Show Card where --showing the card only if its facing up
    show (Card pip suit True) = "(" ++ show pip ++ "," ++ show suit ++ ")"
    show (Card _ _ False) = "<unknown>"

type Deck = [Card]

{- FOUNDATIONS
8-off only, represented as list of maximum 4 cards (one for each suit)
each card represents a stack of cards of the same suit from Ace up to the card, initially empty,
8-off taking: not allowed
8-off puting: always if card is Ace, just append the list or 
    if card is successor of one of the cards already stored in foundations.
Spider taking: not allowed
Spider puting: only if one of the columns has complete suit (from Ace to King)
-}
type Foundations = Deck

{- COLUMN
8-off and Spider, each column is list of Cards,
8-off taking: only top card,
8-off putting: only if head of the column is successor of the card that player wants to put

Spider taking: possible to take first n card if they form a sequence,
Spider putting: possible if head of the column is successor of the last card in sequence that player wants to put
-}
type Column = Deck

{- RESERVES
8-off only, list of maximum 8 cards
taking: possible to take every card
putting: possible with every card if there is free slot, (lenght of reserves is smaller than 8)
-}
type Reserves = Deck

{- STOCK
Spider only, list of initially 50 cards, all cards in stock are hidden,
taking: only by dealing cards from stock: if there are no usefull moves and no column is empty, 
take ten cards from stock and put one in each column, its possible to deal 5 times during game.
putting: not allowed
-}
type Stock = Deck

{- BOARD
8-off consists of Foundations, list of Columns and Reserves
Spider consists of Foundations, list of Columns and Stock
-}
data Board = EOBoard Foundations [Column] Reserves | SBoard Foundations [Column] Stock
    deriving (Eq) --working as expected without manual instantiating 


--helper functions that Shows the columns, each column in new line
showColumns :: [Column] -> String
showColumns [] = ""
showColumns (x:xs) = "\n    " ++ show x  ++ showColumns xs

instance Show Board where
    show (EOBoard f cs r) = "\n\nEOBoard\n" ++ "Foundations " ++ show f
        ++ "\nColumns" ++ showColumns cs ++ "\n\nReserve    " ++ show r ++ "\n\n"


    show (SBoard f cs s) = "\n\nSBoard\n" ++ "Foundations " ++ show f
        ++ "\nColumns" ++ showColumns cs
        ++ "\nStock " ++ show (remainingStockDeals s) ++ " Deals remaining \n\n"
            where
                --stock have initialy 50 cards and then each time 10 cards will be dealt
                remainingStockDeals :: Stock -> Int
                remainingStockDeals s = length s `div` 10


eOExample :: Board --from appendix A, hard coded test example for 8-off Solitaire
eOExample = EOBoard []
                        [[Card Ace Clubs True, Card Seven Diamonds True, Card Ace Hearts True, Card Queen Hearts True, Card King Clubs True, Card Four Spades True],
                        [Card Five Diamonds True, Card Queen Spades True, Card Three Diamonds True, Card Five Spades True, Card Six Spades True, Card Seven Hearts True],
                        [Card King Hearts True, Card Ten Diamonds True, Card Seven Spades True, Card Queen Diamonds True, Card Five Hearts True, Card Eight Diamonds True],
                        [Card Jack Spades True, Card Six Hearts True, Card Seven Clubs True, Card Eight Spades True, Card Ten Clubs True, Card Queen Clubs True],
                        [Card Ace Spades True, Card Eight Clubs True, Card Ace Diamonds True, Card King Diamonds True, Card Jack Hearts True, Card Four Clubs True],
                        [Card Two Diamonds True, Card Three Hearts True, Card Three Clubs True, Card Ten Hearts True, Card Six Diamonds True, Card Jack Clubs True],
                        [Card Nine Spades True, Card Four Diamonds True, Card Nine Clubs True, Card Nine Hearts True, Card Three Spades True, Card Ten Spades True],
                        [Card Two Clubs True, Card Two Spades True, Card Four Hearts True, Card Nine Diamonds True, Card King Spades True, Card Eight Hearts True]]

                        [Card Two Hearts True, Card Six Clubs True, Card Five Clubs True, Card Jack Diamonds True]


sExample :: Board --from appendix B, hard coded test example for Spider Solitaire
sExample = SBoard [Card King Hearts True]
                        [[Card Eight Diamonds True, Card Nine Hearts True],
                        [Card Two Diamonds True],
                        [Card Ace Spades True, Card Two Spades True, Card Three Spades True, Card Four Spades True, Card Five Spades True, Card Six Clubs True, Card Seven Clubs True, Card Eight Clubs True, Card Nine Clubs True, Card Ten Diamonds True, Card Jack Diamonds True, Card Queen Diamonds True, Card King Diamonds True, Card Six Hearts False, Card Eight Hearts False],
                        [Card Seven Clubs True, Card Eight Diamonds True, Card Nine Diamonds True, Card Ten Diamonds True, Card Jack Diamonds True, Card Queen Diamonds True, Card King Diamonds True, Card Nine Clubs True, Card Ten Hearts True, Card Jack Clubs True],
                        [Card Ace Hearts True, Card Two Hearts True, Card Three Hearts True, Card Four Hearts True, Card Five Hearts True, Card Six Diamonds True, Card Seven Diamonds True, Card Queen Clubs True, Card King Hearts True],
                        [Card Two Diamonds True, Card Three Diamonds True, Card Four Diamonds True],
                        [Card Jack Clubs True, Card Queen Clubs True, Card King Clubs True, Card Two Spades True, Card Three Spades True, Card Four Diamonds True, Card Five Diamonds True, Card Six Diamonds True, Card Seven Hearts True, Card Eight Clubs True, Card Nine Spades True, Card Ten Clubs True, Card Ace Clubs True, Card Two Clubs True, Card Three Clubs True, Card Four Clubs True, Card Five Spades True],
                        [Card Seven Spades True, Card Eight Spades True, Card Nine Spades True, Card Ten Spades True, Card Jack Spades True, Card Queen Spades True, Card King Spades True, Card Ace Diamonds False, Card Five Diamonds False, Card Nine Diamonds False],
                        [Card Jack Hearts True, Card Queen Hearts True],
                        [Card Ace Clubs True, Card Two Clubs True]]

                        [Card Six Spades False, Card Seven Spades False, Card Eight Spades False, Card Ten Spades False, Card Jack Spades False, Card Queen Spades False, Card King Spades False, Card Five Clubs False, Card Ace Diamonds False, Card Three Diamonds False, Card Seven Diamonds False, Card Ace Spades False, Card Four Spades False, Card Six Spades False, Card Three Clubs False, Card Four Clubs False, Card Five Clubs False, Card Six Clubs False, Card Ten Clubs False, Card King Clubs False]

--returns successor of provided card
sCard :: Card -> Card
sCard (Card King s v) = Card Ace s v --if provided card is a King return Ace
sCard (Card pip s v) = Card (succ pip) s v

--returns predecessor of provided card
pCard :: Card -> Card
pCard (Card Ace s v) = Card King s v --if provided card is an Ace return King
pCard (Card pip s v) = Card (pred pip) s v

isAce :: Card -> Bool
isAce (Card pip _ _) = pip == Ace

isKing :: Card -> Bool
isKing (Card pip _ _) = pip == King

getSuit :: Card -> Suit
getSuit (Card _ s _) = s

getPip :: Card -> Pip
getPip (Card p _ _) = p

--shuffles deck of cards
--takes integer as input seed to random number generator
--returns shuffled deck of cards
shuffle :: Int -> Deck -> Deck
shuffle seed xs = [ x | (x, n) <- sortBy (\ (_,n1) (_,n2) -> compare n1 n2)
                          (zip xs (randoms (mkStdGen seed) :: [Int]))]

pack :: Deck --creates sorted list of all cards, all of them are initially visible
pack = [Card pip suit True | pip <- [Ace ..], suit <- [Hearts ..]] --[Ace ..] is list of all Pips, [Hearts ..] is list of all Suits

--deals initial 8-off board, gets seed as parameter for shuffling
eODeal :: Int -> Board
eODeal seed = deal (shuffle seed pack)
    where
    deal :: Deck -> Board --gets shuffled cards
    deal deck = EOBoard [] columns reserve --foundations initialy empty
        where
        reserve = take 4 deck --take 4 first cards to reserve
        columns = dealColumns (drop 4 deck) -- deal columns with the rest of the cards
            where
            dealColumns :: Deck -> [Column]
            dealColumns [] = [] --stop if u have no more cards
            dealColumns cards = take 6 cards : dealColumns(drop 6 cards) --take 6 cards for one column and deal with the rest

--deals initial Spider board, gets seed as parameter for shuffling
sDeal :: Int -> Board
sDeal seed = (deal.hideAllCards) (shuffle seed (pack++pack)) --using two decks for spider Solitaire, hidesAll cards initially
    where

    deal :: Deck -> Board --gets 2 shuffled decks
    deal deck = SBoard [] columns stock
        where
        stock = take 50 deck --50 cards go to the stock
        columns = dealColumns (drop 50 deck) --deal columns with the rest
            where
            dealColumns :: Deck -> [Column]
            dealColumns [] = []
            dealColumns cards
                | length cards `mod` 5 /= 0 = showHead(take 6 cards):dealColumns (drop 6 cards) --for first 4 folumns
                | otherwise = showHead(take 5 cards): dealColumns (drop 5 cards) --for last 6 columns

showHead :: Deck -> Deck --shows the first card in column
showHead [] = []
showHead ((Card p s _):rest) = Card p s True:rest

hideAllCards :: Deck -> Deck --puts all cards face down
hideAllCards = map (\ (Card p s _) -> Card p s False)

showAllCards :: Deck -> Deck --puts all cards face up
showAllCards = map (\ (Card p s _) -> Card p s True)

{-
Defining building blocks, to help implement further functionality.
Helper functions that move cards around. 
-}

{-PUT CARD FUNCTIONS-}

--gets card and board and tries to put it to foundations
--returns Nothing is didn't succeed or Just new board
putCardToFoundations :: (Card,Board) -> Maybe Board
putCardToFoundations (card@(Card pip _ _), EOBoard fs cols rs)
    | isAce card = Just (EOBoard (card:fs) cols rs) --its always possible to add ace to foundations
    | otherwise = if newFs == fs then Nothing else Just (EOBoard newFs cols rs)
    where
        --try to add card to each foundations slots
        newFs = map (\fCard -> if card == sCard fCard then card else fCard) fs

--gets card and board and tries to put it to reserves
--returns Nothing is reserves limit reached
putCardToReserve :: (Card, Board) -> Maybe Board
putCardToReserve (card, EOBoard fs cols rs)
    | length rs == 8 = Nothing
    | otherwise = Just (EOBoard fs cols (card:rs))

--gets column index, card and the board
--returns Just new board if its possible to add card to the column of given index
putCardToColumn ::  Int -> (Card, Board) -> Maybe Board
putCardToColumn index (card, EOBoard fs cols rs)
    | null selectedCol = if isKing card then Just newB else Nothing --its possible to add king only to empty column
    | card == pCard(head selectedCol) = Just newB --check if we can put card to the column
    | otherwise = Nothing
        where
            selectedCol = cols!!index
            newB = EOBoard fs newCols rs
            --new columns are the new column with inserted card plus all the other untouched ones
            newCols = beforeIndex ++ [newColumn] ++ afterIndex
            --order of the columns is preserved
            beforeIndex = take index cols
            newColumn = card:selectedCol
            afterIndex = drop (index+1) cols
{-TAKE CARD FUNCTIONS-}

--gets reserve index, board and returns tuple of taken card and new board
takeCardFromReserve :: Int -> Board -> (Card,Board) --if index is in bounds can always take card from reserve
takeCardFromReserve index b@(EOBoard fs cols rs) = (takenCard,newB)
    where
        takenCard = rs!!index
        newB = EOBoard fs cols newRs
        newRs = filter (/= takenCard) rs

--gets column index, board, and returns Maybe a tuple of taken card and new board
takeCardFromColumn :: Int -> Board -> Maybe (Card,Board)
takeCardFromColumn index b@(EOBoard fs cols rs)
    | null (cols!!index) = Nothing --if the column from which we take the card is empty, we cannot take any card thus return Nothing
    | otherwise = Just (takenCard,newB)
        where
            takenCard = head (cols!!index) --take wanted card
            newB = EOBoard fs newCols rs

            --New columns are the tail of the column that we have taken card from plus other all untouched collumns
            newCols = beforeIndex ++ [indexColumn] ++ afterIndex
            --order of the columns is preserved
            beforeIndex = take index cols
            indexColumn = tail (cols!!index)
            afterIndex = drop (index+1) cols

{-SPIDER BUILDING BLOCKS-}

putCardsToColumn ::  Int -> (Deck, Board) -> Maybe Board
putCardsToColumn index (cards, SBoard fs cols stock)
    | isKing topCard = if null selectedCol then Just newB else Nothing
    | canPut = Just newB
    | otherwise = Nothing
        where
            selectedCol = cols!!index

            --we can put card to column when either its emprty, or there is successor pip of sequence that we want to put
            canPut = null selectedCol || succ topCardPip == (getPip.head) selectedCol

            topCard = last cards
            topCardPip = getPip topCard

            newB = SBoard fs newCols stock

            --new columns are the new column with inserted cards plus all the other untouched ones
            newCols = beforeIndex ++ [newColumn] ++ afterIndex
            --order of the columns is preserved
            beforeIndex = take index cols
            newColumn = cards++selectedCol
            afterIndex = drop (index+1) cols

takeCardsFromColumn :: Int -> Int -> Board -> Maybe (Deck,Board)
takeCardsFromColumn index nCardsToTake (SBoard fs cols stock)
    | canTake = Just (takenCards, newB)
    | otherwise = Nothing
        where
            selectedCol = cols!!index
            takenCards = take nCardsToTake selectedCol
            canTake = inOrder takenCards

            inOrder :: Deck -> Bool
            inOrder [x] = True
            inOrder (x:y:rest) = (x == pCard y) && inOrder (y:rest)

            newB = SBoard fs newCols stock

            newCols = beforeIndex ++ [indexColumn] ++ afterIndex
            --order of the columns is preserved
            beforeIndex = take index cols
            indexColumn = showHead(drop nCardsToTake selectedCol)
            afterIndex = drop (index+1) cols




{-TO FOUNDATIONS-}

--autoplay, performs all possible moves to the foundations, works only for 8-off
toFoundations :: Board -> Board
toFoundations b@(EOBoard fs cols rs)
    | null movesToFoundations = b --no card was added to foundations
    | otherwise = toFoundations(head movesToFoundations) --some card was added to foundations we need to check next board
    where
        --find moves from Reserve and from Columns to foundations
        --function calls "head  movesToFoundations" so 
        --list comprehension will stop after first legal move found due to lazy evaluation
        movesToFoundations = movesToFoundationsFromReserve ++ movesToFoundationsFromColumns
        movesToFoundationsFromReserve =
            [
                fromJust newBoard |
                reservesIndex <- [0..length rs - 1],
                let fromReserves = takeCardFromReserve reservesIndex b,
                let newBoard = putCardToFoundations fromReserves, --take each card from reserves and try to put it to foundations
                isJust newBoard
            ]
        movesToFoundationsFromColumns =
            [
                fromJust newBoard |
                columnsIndex <- [0..length cols - 1],
                let fromColumn = takeCardFromColumn columnsIndex b, --try to take each head of the columns
                isJust fromColumn,
                let newBoard = putCardToFoundations (fromJust fromColumn), --try to put that card into foundations
                isJust newBoard
            ]

toFoundations b@(SBoard fs cols stock)
    | null movesToFoundations = b --no cards was added to foundations
    | otherwise = toFoundations(head movesToFoundations) --some cards was added to foundations we need to check next board
    where
        movesToFoundations =
            [
                fromJust newBoard |
                columnsIndex <- [0..length cols - 1],
                let newBoard = putCompleteSuitToFoundations columnsIndex b,
                isJust newBoard
            ]

putCompleteSuitToFoundations :: Int -> Board -> Maybe Board
putCompleteSuitToFoundations index (SBoard fs cols stock)
    | length selectedCol < 13 = Nothing --if there are less cards in selected column than 13 there can't be complete suit
    | completeSuit = Just (SBoard newFs newCols stock)
    | otherwise = Nothing
        where
            selectedCol = cols!!index
            cardsToCheck = take 13 selectedCol

            suit = (getSuit.head) cardsToCheck
            expectedSequence = [(pip,suit) | pip <- [Ace ..]]
            zipped = zip expectedSequence cardsToCheck

            completeSuit = all (\((pip,suit),Card p s _) -> pip==p && suit==s) zipped

            king = last cardsToCheck
            newFs = king:fs

            newCols = beforeIndex ++ [indexColumn] ++ afterIndex
            --order of the columns is preserved
            beforeIndex = take index cols
            indexColumn = showHead (drop 13 selectedCol)
            afterIndex = drop (index+1) cols

{-FIND MOVES FUNCTIONS-}

findMovesFromReservesToColumns :: Board -> [Board]
findMovesFromReservesToColumns b@(EOBoard fs cols rs) =
    [
        fromJust newB |

        reservesIndex <- [0..length rs - 1],
        columnsIndex <- [0..length cols - 1], --all combinations of reserves and columns indeces

        let fromReserve = takeCardFromReserve reservesIndex b, --take card from reserve
        let newB = putCardToColumn columnsIndex fromReserve, --try to put it to column

        isJust newB
    ]

findMovesFromColumnsToReserves :: Board -> [Board]
findMovesFromColumnsToReserves b@(EOBoard fs cols rs) =
    [
        ret | index <- [0..length cols - 1], --all column indexes

        let fromColumn = takeCardFromColumn index b, --take card from column
        isJust fromColumn, --check if taking card was successfull

        let (takenCard, tempB) = fromJust fromColumn,
        let newB = putCardToReserve (takenCard, tempB), --add taken card to reserve

        isJust newB, --check if adding card was successfull
        let ret@(EOBoard _ newCols rs) = fromJust newB,

        --discard move that is taking king from column, that would cause column to be empty,
        not(isKing takenCard && null (newCols!!index))
    ]


findMovesFromColumnsToColumns :: Board -> [Board]
findMovesFromColumnsToColumns b@(EOBoard fs cols rs) =
    [
        fromJust newB |

        let indexBound = length cols - 1,
        fromIndex <- [0..indexBound], --column from which we take card
        toIndex <- [0..indexBound], --column to which we put card

        fromIndex /= toIndex, --avoding situation that we take and put card to the same column

        let fromColumn = takeCardFromColumn fromIndex b, --try to take card from one column
        isJust fromColumn,

        let p@(takenCard,_) = fromJust fromColumn,
        not(isKing takenCard), --don't move king from column to column

        let newB = putCardToColumn toIndex p, --try to put card into other column
        isJust newB

    ]

findMovesFromColumnsToColumns b@(SBoard fs cols rs) =
    [
        fromJust newB |

        let indexBound = length cols - 1,
        fromIndex <- [0..indexBound], --column from which we take card
        toIndex <- [0..indexBound], --column to which we put card

        fromIndex /= toIndex,

        let maxNumOfCardsToTake = length (cols!!fromIndex),
        nCardsToTake <- [1..maxNumOfCardsToTake],

        let fromColumn = takeCardsFromColumn fromIndex nCardsToTake b,
        isJust fromColumn,

        let newB = putCardsToColumn toIndex (fromJust fromColumn),
        isJust newB
    ]


dealStock :: Board -> Maybe Board
dealStock b@(SBoard fs cols stock)
    | null stock || any null cols = Nothing --if stock is empty of any column is emprty cannot deal from stock
    | otherwise = Just (SBoard fs newCols newStock)
    where
        cardsToDeal = showAllCards (take 10 stock) --put cards face up when they are taken from stock
        newStock = drop 10 stock

        zipped = zip cols cardsToDeal
        newCols = map (\ (col,card) -> card:col ) zipped


--find all possible legal moves given board
findMoves :: Board -> [Board]
findMoves b@EOBoard {} = map toFoundations allMoves --the last step in any move should be a call to toFoundations
    where
        --there are three types of moves
        allMoves =  findMovesFromReservesToColumns b ++
                    findMovesFromColumnsToColumns b ++
                    findMovesFromColumnsToReserves b

findMoves b@SBoard {} = map toFoundations allMoves
    where
        --two types of move from column to column and deal from stock
        allMoves = findMovesFromColumnsToColumns b ++ dealStockMove
        maybeDealStock = dealStock b
        dealStockMove = [fromJust maybeDealStock | isJust maybeDealStock] --if dealing from stock is possible add it to the list moves

--take board and pick best next move from list of all legal possibilities
chooseMove :: Board -> Maybe Board
chooseMove b
    | null filteredMoves = Nothing
    | otherwise = Just move --take first move valid move
        where
            allFirstMoves = findMoves (toFoundations b)
            allSecondMoves = map (findMoves.toFoundations) allFirstMoves --list of lists
            zipped = zip allFirstMoves allSecondMoves
            --check if b doesnt appear in any next moves
            filteredMoves = filter (\ (_,nextMoves) -> b `notElem` nextMoves) zipped
            move = (fst.head) filteredMoves


--counts how many times card was added to foundations, maximum 52
calculateScore :: Board -> Int
calculateScore (EOBoard fs _ _) =
    foldr (\(Card pip _ _) accumulator -> fromJust (elemIndex pip [Ace ..])+1+accumulator) 0 fs
calculateScore (SBoard fs _ _) = 13 * length fs

--game is won when all cards are in foundations which is calculates score equals 52 for 8-off and 104 for Spider
haveWon :: Board -> Bool
haveWon b@EOBoard {} = calculateScore b == 52
haveWon b@SBoard {} = calculateScore b == 104

--playes game of solitaire untill there are no legal moves
--returns achieved score
playSolitaire :: Board -> Int
playSolitaire b
  | isNothing nextB = calculateScore b --game over
  | otherwise = playSolitaire (fromJust nextB) --keep playing
      where
      nextB = chooseMove b

{-Building blocks for Analyse functions-}

data GameType = EO | Spider
instance Show GameType where
    show EO = "Eight-Off"
    show Spider = "Spider"


getWinningScore :: GameType -> Int
getWinningScore EO = 52
getWinningScore Spider = 104

getDeal :: GameType -> (Int -> Board)
getDeal EO = eODeal
getDeal Spider = sDeal


analyse :: GameType -> Int -> Int -> IO(Int, Float)
analyse gameType initialSeed nGames =
    do
        putStrLn $ "Analysing performance of " ++ show gameType ++ " Solitaire game."
        putStrLn $ "Games to be played: " ++ show nGames

        seeds <- newChan --creating channel for game seeds
        --generating random seeds for games and writing them to seeds channel
        writeList2Chan seeds (take nGames (randoms (mkStdGen initialSeed) :: [Int]))
        scores <- newChan --here game score will be stored

        --starting threads nGames threads
        mapM_ (\_ -> forkIO $! threadPlayGame gameType seeds scores) [1..nGames]

        putStrLn "Playing games..."

        --waiting for threads
        allResults <- mapM (\_ -> readChan scores) [1..nGames]

        --calculeting result
        let winningScore = getWinningScore gameType
        let wonGames = foldr (\x acc -> if x==winningScore then x+acc else acc) 0 allResults
        let averageScore = fromIntegral (sum allResults) / fromIntegral nGames

        putStrLn $ "All " ++ show nGames ++ " have been played"
        putStrLn $ "Won games: " ++ show wonGames
        putStrLn $ "Average score: " ++ show averageScore

        return (wonGames,averageScore)


threadPlayGame :: GameType -> Chan Int -> Chan Int -> IO ()
threadPlayGame gameType seeds scores =
    do
        seed <- readChan seeds
        let board = getDeal gameType seed
        writeChan scores (playSolitaire board)

--using general function to analyse particular game types
analyseEO = analyse EO
analyseSpider = analyse Spider

studentName = "Aleksander Marcin Osikowicz"
studentNumber = "200130914"
studentUsername = "aca20amo"

initialBoardDefined = eOExample
secondBoardDefined = sExample

main :: IO()
main =
    do
        putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

        putStrLn "***The eight-off initial board constant from part 1:"
        print initialBoardDefined

        let board = toFoundations initialBoardDefined
        putStrLn "***The result of calling toFoundations on that board:"
        print board

        let boards = findMoves board      -- show that findMoves is working
        putStrLn "***The possible next moves after that:"
        print boards

        let chosen = chooseMove board     -- show that chooseMove is working
        putStrLn "***The chosen move from that set:"
        print chosen

        putStrLn "***Now showing a full game"     -- display a full game
        score <- displayGame initialBoardDefined 0
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


        putStrLn "\n\n\n************\nNow looking at the alternative game:"

        putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
        print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won


        putStrLn "***Now showing a full game for alternative solitaire"
        score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
        putStrLn $ "Score: " ++ score
        putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)


displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
    then return "A WIN"
    else
        do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                    let (Just newBoard) = maybeBoard
                    displayGame newBoard (n+1)
            else
                do
                    let score = show (playSolitaire board)
                    return score
