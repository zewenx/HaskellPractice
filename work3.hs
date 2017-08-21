-- @Author: anchen
-- @Date:   2017-08-18 15:42:08
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-18 16:06:12

mergeList :: Ord t => [t] -> [t] -> [t]

mergeList [] [t] = [t]
mergeList [t] [] = [t]
mergeList (a:as) (b:bs)
 | a < b = [a] ++ mergeList as (b:bs)
 | b < a = [b] ++ mergeList (a:as) bs
 | otherwise = [a] ++ [b] ++ mergeList as bs