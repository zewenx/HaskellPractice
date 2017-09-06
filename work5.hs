-- @Author: anchen
-- @Date:   2017-09-06 09:59:02
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-09-06 11:33:57

import Data.List
-- lecture week 5

--what is the purpose of using a function as argument?

filters ::(a ->Bool) -> [a] -> [a]
filters f [] = []
filters f (a:as) 
    | f a = a : ffx
    | otherwise = ffx
    where ffx = filters f as

isPositive :: (Num a, Ord a) => a -> Bool
isPositive a 
    | a > 0 = True
    | otherwise = False 

isPositive_ :: (Num a, Ord a) => a -> Bool
isPositive_ a = a > 0 
    
isEven :: Int -> Bool
isEven a = (mod a 2) ==0

isEven_ :: Int -> Bool
isEven_ a = a `mod` 2 ==0

anonymouts = filters ( \x -> x `mod` 2 == 0) [2,-1,0,4,6,7,-3,9,-4]


generateGuessList :: [String]-> Int->[[String]]
generateGuessList [] _ =  [[]]
generateGuessList (a:as) i 
    | i <=0 =[[]]
    | length (a:as) < i = [[]]
    | length (a:as) == i = [(a:as)]
    | otherwise = (map ([a]++) (generateGuessList as (i-1) ) )++ (generateGuessList as i)

ss = map (1+) [1,2,3,4]

ff2 :: [s] -> [(s,s)]
ff2 (a:as) 
    | length (a:as) < 2 = []
    | otherwise = [(a,b)|b<-as]++ ff2 as

ff3 ::Eq s => [s] -> [(s,s,s)]
ff3 x
    | (length x) < 3 = []
    | otherwise = [(a,b,c)|a<-x,b<-x,c<-x, a/=b, b /=c , a/= c]

--* ????
--get_names1 :: [Customer] -> [String]
--get_names1 customers = map customer_name customers
--
--get_names2 :: [Customer] -> [String]
--get_names2 = map customer_name 
--