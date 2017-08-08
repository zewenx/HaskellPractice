-- @Author: zewen xu
-- @Date:   2017-08-04 17:16:41
-- @Last Modified time: 2017-08-04 20:16:16


lens :: [a]->Int
lens [] = 0
lens (a:as) = 1 + lens as


all_pos (s:ss) = s > 0 && all_pos ss
all_pos [] = True
posNum = all_pos [4,2]


kkp :: [s]->[s]->[s]
kkp [] b = b
kkp (a:ass) b = a:kkp ass b

revs :: [q]->[q]
revs [] = []
revs (s:ss) = kkp (revs ss) [s]

+++ :: [s]->[s]->[s]
+++ [] b = b
+++ (a:ass) b = a:+++ ass b