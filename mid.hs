-- @Author: anchen
-- @Date:   2017-09-13 22:44:51
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-09-13 23:37:33

data Mtree a = Mnode a [Mtree a]

showMtree :: Show a => Mtree a -> String
showMtree = showLength 0

showLength ::Show a => Int -> Mtree a -> String
showLength x (Mnode m t)
    | length t == 0 = (spaceX x) ++ show m ++"\n"
    | otherwise = (spaceX x) ++ show m ++"\n" ++ (foldl (++) "" (map (showLength (x+1)) t))

spaceX :: Int -> String
spaceX x 
    | x == 0 = ""
    | otherwise = " "++ spaceX (x-1)


showMtree1 :: Show a => Mtree a -> String
showMtree1 tree = showMtree' "" tree

showMtree' :: Show a => String -> Mtree a -> String
showMtree' indent (Mnode label subtrees) =
  indent ++ show label ++ "\n" ++ concatMap (showMtree' (' ':indent)) subtrees
