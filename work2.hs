-- @Author: anchen
-- @Date:   2017-08-11 15:37:23
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-11 16:12:47

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


