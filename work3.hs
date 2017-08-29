-- @Author: anchen
-- @Date:   2017-08-18 15:42:08
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-24 22:02:17

--workshop 3
mergeList :: Ord t => [t] -> [t] -> [t]

mergeList [] s = s
mergeList s [] = s
mergeList (a:as) (b:bs)
 | a < b = [a] ++ mergeList as (b:bs)
 | b < a = [b] ++ mergeList (a:as) bs
 | a==b = [a] ++ [b] ++ mergeList as bs


--week 3 lecture
data PubFreq = Days Int | Months Int
data LibItem = Book Int String String | Periodical Int String PubFreq

title :: LibItem -> String
title (Book a b c) = b
title (Periodical a b c) = b

a = Book 2 "tt" "ss"
b = Periodical 3 "bb" (Months 4)

data Lib = Maybe LibItem

data Maybe t = Nothing | Just PubFreq
ft = Months 3

isDailyPub :: LibItem -> Bool
isDailyPub libItem =
    case libItem of 
        Book _ _ _ -> False
        Periodical _ _ pb -> isDayPub pb
isDayPub :: PubFreq -> Bool 
isDayPub pub = 
    case pub of 
        Days _ -> True
        Months _ -> False


someLarger :: [Int]->Int->Int
someLarger (a:as) size
    | size == 1 = 3*a
    | a>0 = anotherLarger as (a*2) (size-1)
    | otherwise = someLarger as (size-1)
anotherLarger :: [Int]->Int->Int->Int
anotherLarger (a:as) target size
    | size-1 ==0 = 3*a
    | a>target = 3*a
    | otherwise = anotherLarger as target (size-1)

find :: [Int]->Int->Int
find (a:as) size = someLarger (a:as) size

filterNeg :: [Int]->[Int]
filterNeg [] = []
filterNeg (s:sx) 
    | s < 0 = filterNeg sx
    |otherwise = s:filterNeg sx