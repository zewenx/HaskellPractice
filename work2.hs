-- @Author: anchen
-- @Date:   2017-08-11 15:37:23
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-25 12:52:48

--workshop 2
factorial :: Int -> Int
factorial n 
    | n==1 = 1
    | n < 0 = error "wrong input"
    | otherwise = n * factorial (n-1)

myEle :: Eq a => a->[a]->Bool
myEle _ [] = False
myEle a (s:ss)
    | a == s = True
    | otherwise = myEle a ss

longestPrefix ::Eq s => [s]->[s]->[s]
longestPrefix (a:as) (b:bs)
    | a == b = [a] ++ longestPrefix as bs
    | otherwise = []

--Q6
translateFromC_Count :: Int->Int->Int
translateFromC_Count n c
    | c == 0 = n
    | c > 0 && n > 100 = translateFromC_Count (n-10) (c-1) 
    | c > 0 && n <=100 = translateFromC_Count (n+11) (c+1)

translateFromC :: Int -> Int
translateFromC a = translateFromC_Count a 1

--Q7
getList :: Int -> Int -> [Int]
getList min max 
    | min < max = min: getList (min +1) max
    | otherwise = [min]




-- week 2 lecture
data Gender = Male | Female


data Suit = Club | Diamond | Heart | Spade deriving Show
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | Jack | Queen | King | Ace deriving (Eq, Ord) 
data Card = Card Suit Rank 

getRank :: Card -> Rank
getRank (Card a b) = b

xxx = Card Club R5
printRank :: Rank -> [Char]
printRank R7 ="7"

printRank2 :: Rank -> String
printRank2 a 
 | a == R2 = "2"
 | otherwise = "7"

oneCard = R2
twoCard = R5

abool = oneCard > twoCard

instance Show Rank where
    show = printRank2

data SpecialCard = Red | Black
data AllCard = NormalCard Suit Rank | SpecialCard SpecialCard
