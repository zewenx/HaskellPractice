-- @Author: anchen
-- @Date:   2017-08-28 17:13:09
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-29 12:13:57

--module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import System.Environment
import System.Exit
import Data.Hashable
import Data.HashMap.Strict



--data Category = "000" | "100" | "010" | "001" | "200" | "020" | "002" | "110" | "101" | "011" | "300" | "030" | "003" | "210" | "201" | "120" | "102" | "012" | "021" | "111" 


data PitchPair = PitchPair String String String

note = ["A","B","C"]
octave = ["1","2","3"]

generatePitchs :: [String]->[String]->[String]
generatePitchs [] s = []
generatePitchs s [] = []
generatePitchs (a:as) (b:bs) = [a++b] ++ (generatePitchs [a] bs) ++ (generatePitchs as [b]) ++ (generatePitchs as bs)

generateGuessList :: [String]-> Int->[[String]]
generateGuessList [] _ =  [[]]
generateGuessList (a:as) i 
    | i <=0 =[[]]
    | length (a:as) < i = [[]]
    | length (a:as) == i = [(a:as)]
    | otherwise = (Data.List.map (a:) (generateGuessList as (i-1) ) )++ (generateGuessList as i)

pitchs = generatePitchs note octave
guessList = generateGuessList pitchs 3

--l1 = length guessList

convertDataType :: (String,String,String)->[String]
convertDataType (a,b,c) = [a]++[b]++[c]


resultCategory :: [String] -> [[String]] ->  [(Int,Int,Int)]
resultCategory target []  = [] 
resultCategory target (a:as)  =
    let (c1,c2,c3) = response target a in
    [(c1,c2,c3)] ++ resultCategory target as


countList :: [(Int,Int,Int)] -> Int
countList xs =
    let mapList = fromListWith (+) [(x,1)|x<-xs] in
    let list = toList mapList in
    maxCount list 0

maxCount :: [((Int, Int, Int),Int)] -> Int->Int
maxCount [] temp = temp
maxCount ((_,d):as) temp 
    | temp < d = maxCount as d
    | otherwise = maxCount as temp


maxx = resultCategory (head guessList) (tail guessList) 
maxxx = countList maxx

bestGuess :: [[String]]->[[String]] -> Int -> [String] -> [String]
bestGuess _ [[]] _ tempGuess = tempGuess
bestGuess from (a:as) tempCount tempGuess = 
        let aResult = countList (resultCategory a (from++as)) in 
        if aResult < tempCount 
            then bestGuess (from++[a]) as aResult a
        else bestGuess (from++[a]) as tempCount tempGuess
best = bestGuess [[]] guessList 1300 []

response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right

eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)
--initialGuess : : ([String],GameState)

--nextGuess :: ( [String],GameState) → (Int,Int,Int) → ([String],GameState)