-- @Author: anchen
-- @Date:   2017-08-25 12:28:42
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-09-13 12:37:35


--rightTriangles m n = [(a, b, c) | c <- [m..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
--mapList = fromListWith (+) [(x,1)|x<-xs] 


--lecture week 4

data Tree = Leaf | Node String Int Tree Tree
--data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)

--countnodes :: Tree -> Int

--countnodes :: Tree k v -> Int

--search_bst :: Tree -> String -> Maybe Int

-- it won't work, k v need EQ or Ord
--search_bst :: Tree k v -> k -> Maybe v

-- Struture Induction and Proof


-- workshop 4
--Q1

--Q2 
transpose :: [[a]] -> [[a]]
transpose []= []
transpose s
    | length (head s) == 0 = []
    | otherwise = (map head s) : transpose (map tail s)

--Q3

stats ::  [Int] -> (Int,Int,Int)
stats s = ((length s), (sum s), sum (map (^2) s))

stats2 [] =(0,0,0)
stats2 (s:ss) = 
    let (l,p,t) = stats2 ss in
    (1+l,p+s,t+s*s)