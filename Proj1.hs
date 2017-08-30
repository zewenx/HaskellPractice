-- @Author:   Zewen Xu
-- @UniLogin: zewenx
-- @Date:     2017-08-28 17:13:09

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Map

-- Define the GameState. It is the remaining potential guess list
data GameState = GameState [[String]]

-- Define notes
note = ["A","B","C","D","E","F","G"]

-- Define ovtaves
octave = ["1","2","3"]

-- Define a variable of pitch list
pitchs = generatePitchs note octave

-- Define a variable for all possible guess
guessList = generateGuessList pitchs 3

-- This function will generate all the pitchs
-- 
-- [String] ---------- notes
-- [String] ---------- octaves
-- [String] ---------- pitches
generatePitchs :: [String]->[String]->[String]
generatePitchs [] s = []
generatePitchs s [] = []
generatePitchs (a:as) (b:bs) = [a++b] ++ (generatePitchs [a] bs) ++ (generatePitchs as [b]) ++ (generatePitchs as bs)


-- This function will generate all the possible guess and put them into a list
-- The first parameter is the pitch list, the second is a number which refers 
-- to how many pitchs are grouped in a guess.
--
-- [String]   ----------- the pitch list
-- Int        ----------- the number of pitch in a guess
-- [[String]] ----------- the guess list
generateGuessList :: [String]-> Int->[[String]]
generateGuessList [] _ =  [[]]
generateGuessList (a:as) i 
    | i <=0 =[[]]
    | length (a:as) < i = [[]]
    | length (a:as) == i = [(a:as)]
    | otherwise = (Data.List.map (a:) (generateGuessList as (i-1) ) )++ (generateGuessList as i)


-- Calculate the feedbacks of all guess in guesslist with target, then put all
-- the feedbacks in a list and return that list
-- 
-- [String] ------------ target
-- [[String]] ---------- guesslist
-- [(Int,Int,Int)] ----- resultList
feedbackList :: [String] -> [[String]] ->  [(Int,Int,Int)]
feedbackList target []  = [] 
feedbackList target (a:as)  =
    let (c1,c2,c3) = response target a in
    [(c1,c2,c3)] ++ feedbackList target as


-- Count the occurrence of each feedback in the feedbacklist and return the 
-- largest number
-- 
-- [(Int,Int,Int)] ----- resultList
-- Int ----------------- largest one
countMaxOccurrence :: [(Int,Int,Int)] -> Int
countMaxOccurrence xs =
    let mapList = fromListWith (+) [(x,1)|x<-xs] in
    let list = toList mapList in
    maxCount list 0


-- This function works with countMaxOccurrence together to get the largest 
-- number of occurrence in feedbacklist
--
-- [((Int, Int, Int),Int)]----- feedbacklist together with its occurrence time
-- Int ------------------------ the template largest number
-- Int ------------------------ the final largest number
maxCount :: [((Int, Int, Int),Int)] -> Int->Int
maxCount [] temp = temp
maxCount ((_,d):as) temp 
    | temp < d = maxCount as d
    | otherwise = maxCount as temp


-- Use max-min search to find the BestGuess as the next guess. General idea is
-- to pick up a guess ( as potential target) in the guesslist and calculate 
-- the feedback of the potential target with every guess in the guesslist. Then
-- group the the guess base on whether the feedback is the same. Count the 
-- number of the largest group. THIS IS THE MAXIMUM NUMBER. Then pick up every 
-- guess as potential target and repeat the process above. And then get the 
-- MAXIMUM NUMBER for each guess. Finally, the guess which has the minimal 
-- MAXIMUM NUMBER would be the BEST GUESS for next guess.
--
-- [[String]] ----------- 'to' list. would be empty in the beginning. A guess 
--                         which has been caculated in 'from' list will be 
--                         moved in this list
-- [[String]] ----------- 'from' list. would contain all guesses in the 
--                        beginning. A guess which has been caculated will be 
--                        moving out.
-- Int        ----------- A temporary variable for count
-- [String]   ----------- A temporary guess
-- [String]   ----------- The final guess
bestGuess :: [[String]]->[[String]] -> Int -> [String] -> [String]
bestGuess _ [] _ tempGuess = tempGuess
bestGuess to (a:as) tempCount tempGuess = 
    let aResult = countMaxOccurrence (feedbackList a (to++as)) in 
    if aResult < tempCount 
        then bestGuess (to++[a]) as aResult a
        else bestGuess (to++[a]) as tempCount tempGuess


-- Initial guess would be ["A1","B1","C2"] following by HINT 4. Initial 
-- GameState will be the whole guess list containing all the 1330 potential 
-- guesses.
--
-- ([String],GameState) ---- Return the guess and current guessList
initialGuess :: ([String],GameState)
initialGuess = (["A1","B1","C2"],GameState guessList)


-- Firstly, it will remove all the guesses which has different feedback with 
-- the previous guess from previous guessing feedback in the GameState. Then 
-- use the max-min search to get the BestGuess as next guess.
--
-- ([String],GameState) ------------ previous guess and remaining guesslist
-- (Int,Int,Int)        ------------ previous feedback
-- ([String],GameState) ------------ next guess and new remaining guesslist
nextGuess :: ([String],GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (currentGuess,GameState (a:as) ) (r1,r2,r3) =
    let GameState newstate = removeInvalidGuess (GameState (a:as)) (GameState []) currentGuess (r1,r2,r3) in
    (bestGuess [] newstate 1330 [],GameState newstate)


-- Caculate the feedback of every potential guess in guesslist with previous 
-- guess. If the feedback is different from the previous feedback, then remove
-- the guess in the guesslist.(Hint 2)
--
-- GameState ----------- 'from' list. Would contain all guesses in the 
--                       beginning. A guess which has different feedback from
--                       previous feedback will be considered as a Valid one
--                       and will be removed. Otherwise it will be add to to 
--                       list
-- GameState ----------- 'to' list. would be empty in the beginning. A valid 
--                       guess will be added to this.
-- [String] ------------ previous guess
-- (Int,Int,Int) ------- previous feedback
-- GameState ----------- the valid guess list
removeInvalidGuess :: GameState ->GameState->[String]->(Int,Int,Int)->GameState
removeInvalidGuess (GameState []) (GameState toList) target (a1,a2,a3) = GameState toList
removeInvalidGuess (GameState (fromList:fromLists)) (GameState toList) target (a1,a2,a3) =
    let (b1,b2,b3) = response fromList target in
    if a1==b1 && a2==b2 && a3==b3
        then removeInvalidGuess (GameState fromLists) (GameState (toList++[fromList])) target (a1,a2,a3)
        else removeInvalidGuess (GameState fromLists) (GameState toList) target (a1,a2,a3)


-- Copy from Proj1Test.hs for caculating the feedback
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right

-- Copy from Proj1Test.hs for caculating the feedback
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)