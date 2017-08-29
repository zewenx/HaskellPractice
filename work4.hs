-- @Author: anchen
-- @Date:   2017-08-25 12:28:42
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-29 22:48:00


rightTriangles m n = [(a, b, c) | c <- [m..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
mapList = fromListWith (+) [(x,1)|x<-xs] in

-- workshop 4
transcope :: [[Int]] -> [[Int]]
transcope (a:as):ss = [(a:transcope ss),transcope as

--lecture week 4

data Tree = Leaf | Node String Int Tree Tree
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)

countnodes :: Tree -> Int

countnodes :: Tree k v -> Int

search_bst :: Tree -> String -> Maybe Int

-- it won't work, k v need EQ or Ord
search_bst :: Tree k v -> k -> Maybe v

-- Struture Induction and Proof