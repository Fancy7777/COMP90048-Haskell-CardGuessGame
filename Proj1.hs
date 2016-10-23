{-
    --Name: Derui Wang
    --Student ID: 679552
    --Purpose: This game initial with user input the amount of cards the he draw from his deck.
               Our job is to use this file to guess the right answer within limited times.
              
               this file basically includes three main functions which are 
                {
                    feedback: compare target with guess and generate five integer feedback tuple 
                    initialGuess: generate initial guess by given cardnumber and initial card list 
                                  stored in gamestate 
                    nextGuess: takes previous guess with a left gamestate and also the feedback 
                               for generateing next guess
                    GameState:store the list of remaining poissble answers. It will be pared down 
                              each time receiving feedback for a guess
                } 
    --My method: 1. Initial guess with choose rank that are about 13/(n+1) apart by given amount 
                    of card number. Their suit are different from each other.
                 2. Then I generate a list of possible answers stored in gameState
                 3. after initial guess. pick the first element of the left possible answers. then 
                    using feedback to generate new possible answers which remove answers that is not
                    consistant with the guess
-}
module Proj1 (feedback, initialGuess, nextGuess, GameState) where
import Card
import Data.List
suitchars = "CDHS"
rankchars = "23456789TJQKA"




--GamesState type is a card list within another list
--this makes easy to pick the first element out as a guess
type GameState = [[Card]]



{-
    Feedback function takes two card list answer and guess and output
    five feedback numbers in a tuple
-}
feedback::[Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback answer guess = 
        (correctAnswer answer guess, 
        lowerRank answer guess, 
        sameRank answer guess, 
        higherRank answer guess, 
        sameSuit answer guess)
{-
    End of feedback function 
-}

{-
    Start of initial guess function 
    including: transform, initialGuessGenerator, 
    buildGameState, buildSequenceable and dupFilter function
-}
initialGuess::Int -> ([Card],GameState)
initialGuess cardNumber = ((initialGuessGenerator (transform cardNumber)),initalGameState)
    where
    initalGameState = initialGameState cardNumber


--help transform givien card number to a list of two same card number
transform::Int -> [Int]
transform cardNumber = [cardNumber, cardNumber]


--take two number to help generate initial guess. It takes a static card number and a 
--changable card number. The changable card number is using for looping to make each guess 
--are seperated by 13/(cardNumber+1) length. The static card number is for calculating 
--the fixing index.
initialGuessGenerator::[Int] ->[Card]
initialGuessGenerator cardSet= 
    if loopNumber /= 0 
        then ((initialGuessGenerator (cardNumber:[(loopNumber-1)]))++[(read (cardRank:cardSuit)::Card)])
        else []
            where 
                cardRank = rankchars!!(((length rankchars)`div` (cardNumber+1))*loopNumber)
                cardSuit = [suitchars!!(loopNumber-1)]
                cardNumber = head cardSet
                loopNumber = last cardSet   


--using helpFilter and makeGameState to initial the whole GameState
initialGameState :: Int -> GameState
initialGameState cardNumber = filter helpFilter initalGameState
    where
    initalGameState = sequence (makeGameState cardNumber)


-- generate a list of all cards for later use
allCard = [minBound..maxBound]


--use makeGameState function to generate several cards list for initialGameState
--to use in the filter function combined with helpFilter
makeGameState :: Int -> [[Card]]
makeGameState 1 = [allCard]
makeGameState cardNumber = allCard:makeGameState (cardNumber-1)
     

--helpFilter function returns False when the list has same cards more than once 
--returns True when the list has no same cards
helpFilter :: Eq a => [a] -> Bool
helpFilter []        = True
helpFilter (x:xs)
    | xs == []      = True
    | x == head xs  = False
    | otherwise     = not (x `elem` xs) && helpFilter xs
{-
    End of initial guess function 
-}



{-
    Start of nextGuess  function 
    including: 
    gameStateGenerator1, gameStateGenerator2, gameStateGenerator3, gameStateGenerator4,
    gameStateGenerator5
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess,gameState) (correctAnswer1,lowerRank1,sameRank1,higherRank1,sameSuit1)  
 = (head finalState,tail finalState)
    where 
        gameState1 = gameStateGenerator1 correctAnswer1 (guess,gameState)
        gameState2 = gameStateGenerator2 lowerRank1 (guess,gameState1)
        gameState3 = gameStateGenerator3 sameRank1 (guess,gameState2)
        gameState4 = gameStateGenerator4 higherRank1 (guess,gameState3)
        finalState = gameStateGenerator5 sameSuit1 (guess,gameState4)


--use corrctAnswer function to take a given feedback value to generate a new gameState for later use
gameStateGenerator1:: Int -> ([Card],GameState) -> GameState
gameStateGenerator1 _ (guess,[]) = []
gameStateGenerator1 n (guess,gameState) = 
    if correctAnswer (head gameState) guess == n
    then head gameState:(gameStateGenerator1 n (guess,tail gameState))
    else gameStateGenerator1 n (guess,tail gameState)


--use lowerRank function to take a given feedback value to generate a new gameState for later use
gameStateGenerator2:: Int -> ([Card],GameState) -> GameState
gameStateGenerator2 _ (guess,[]) = []
gameStateGenerator2 n (guess,gameState) = 
    if lowerRank (head gameState) guess  == n
    then head gameState:(gameStateGenerator2 n (guess,tail gameState))
    else gameStateGenerator2 n (guess,tail gameState)


--use sameRank function to take a given feedback value to generate a new gameState for later use
gameStateGenerator3:: Int -> ([Card],GameState) -> GameState
gameStateGenerator3 _ (guess,[]) = []
gameStateGenerator3 n (guess,gameState) = 
    if sameRank (head gameState) guess == n
    then head gameState:(gameStateGenerator3 n (guess,tail gameState))
    else gameStateGenerator3 n (guess,tail gameState)


--use higherRank function to take a given feedback value to generate a new gameState for later use
gameStateGenerator4:: Int -> ([Card],GameState) -> GameState
gameStateGenerator4 _ (guess,[]) = []
gameStateGenerator4 n (guess,gameState) = 
    if higherRank (head gameState) guess == n
    then head gameState:(gameStateGenerator4 n (guess,tail gameState))
    else gameStateGenerator4 n (guess,tail gameState)


--use sameSuit function to take a given feedback value to generate a new gameState for later use
gameStateGenerator5:: Int -> ([Card],GameState) -> GameState
gameStateGenerator5 _ (guess,[]) = []
gameStateGenerator5 n (guess,gameState) = 
    if sameSuit (head gameState) guess == n
    then head gameState:(gameStateGenerator5 n (guess,tail gameState))
    else gameStateGenerator5 n (guess,tail gameState)
{-
    End of nextGuess  function 
-}



{-
    Start of feedback helper function 
    including: 
    correctAnswer, lowerRank, sameRank with its helper function rankHelp, higherRank and 
    sameSuit with its help function suitHelp
-}

--check how many cards in the answer are also in the guess
correctAnswer::[Card] -> [Card] -> Int
correctAnswer [] card_lst = 0
correctAnswer (x:xs) guess= 
    if x `elem` guess 
        then (1+correctAnswer xs guess) 
        else correctAnswer xs guess


--check how many cards in the answer have rank lower than the lowest rank in the guess
lowerRank::[Card] -> [Card] -> Int
lowerRank [] guess = 0
lowerRank (x:xs) guess = 
    if (rank x<minimumInGuessRank) 
        then 1+ (lowerRank xs guess) 
        else lowerRank xs guess
            where minimumInGuessRank = minimum (map rank guess)


--check how many of cards in the answer have same rank as a card in the guess
--each card in the guess only counted once
sameRank::[Card] -> [Card] -> Int
sameRank answer guess = rankHelp (map rank answer) (map rank guess)


--compute same ranks using rank_help function. Delete elements that already been
--compared from guess. 
rankHelp::[Rank] -> [Rank] -> Int
rankHelp [] guess = 0
rankHelp (x:xs) guess = 
    if x `elem` guess 
        then 1+ rankHelp xs (delete x guess) 
        else rankHelp xs guess


--check how many cards in the answer have rank higher than the highest rank in the guess
higherRank::[Card] -> [Card] -> Int
higherRank [] guess = 0
higherRank (x:xs) guess= 
    if (rank x>maximumInGuessRank) 
        then 1+ (higherRank xs guess) 
        else higherRank xs guess
            where maximumInGuessRank = maximum (map rank guess)


--check how many of cards in the answer have same suit as a card in the guess
--each card in the guess only counted once
sameSuit::[Card] -> [Card] -> Int
sameSuit answer guess = suit_help (map suit answer) (map suit guess)


--compute same suits using suit_help function. Delete elements that already been
--compared from guess. 
suit_help::[Suit] -> [Suit] -> Int
suit_help [] guess = 0
suit_help (x:xs) guess = 
    if x `elem` guess 
        then 1+ suit_help xs (delete x guess) 
        else suit_help xs guess

{-
    End of feedback helper function
-}