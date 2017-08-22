-- @Author: anchen
-- @Date:   2017-08-18 15:42:08
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-22 23:06:00

mergeList :: Ord t => [t] -> [t] -> [t]

mergeList [] [t] = [t]
mergeList [t] [] = [t]
mergeList (a:as) (b:bs)
 | a < b = [a] ++ mergeList as (b:bs)
 | b < a = [b] ++ mergeList (a:as) bs
 | otherwise = [a] ++ [b] ++ mergeList as bs



data PubFreq = Days Int | Months Int
data LibItem = Book Int String String | Periodical Int String PubFreq

title :: LibItem -> String
title (Book a b c) = b
title (Periodical a b c) = b

a = Book 2 "tt" "ss"
b = Periodical 3 "bb" (Days 4)