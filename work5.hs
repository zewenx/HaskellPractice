-- @Author: anchen
-- @Date:   2017-09-06 09:59:02
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-09-13 14:00:09


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

getInt :: Int -> Int ->Int
getInt = getNum

getNum :: Int -> Int -> Int 
getNum a b = if (a>b) then a else b

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

--

--get_names1 :: [Customer] -> [String]
--get_names1 customers = map customer_name customers
--
--get_names2 :: [Customer] -> [String]
--get_names2 = map customer_name 
--

--
--reverse . map (1+) [2,3,5,1,4]
--reverse . sort [2,3,5,1,4]

--concatr is more efficient than concatl

square x = x ^ 2
hypotenuse ::Floating a => [a] -> a
hypotenuse sides = sqrt (sum (map square sides))

hypotenuse' ::Floating a => [a] -> a
hypotenuse' = sqrt . sum . (map square)

-- workshop5
--q1
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing = Nothing
maybeApply f (Just a) = Just $ f a
-- what is the function of $

--q2
zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith f [] _ = []
zWith f _ [] = []
zWith f  a b =
    f (head a) (head b) : zWith f (tail a) (tail b)

--Q3
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn a b = map (\x -> a*x + b ) 

linearEqn1 a b = map (cal a b)

cal :: Num a =>a->a-> a -> a
cal a b x= a*x +b 

--Q4
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
  | x  > 0    = let y = sqrt x in [y, -y] 
  | x == 0    = [0]
  | otherwise = []


allSqrts s= foldl (++) [] (map sqrtPM s) 
