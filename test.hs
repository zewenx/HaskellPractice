-- @Author: anchen
-- @Date:   2017-08-28 20:21:46
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-28 20:43:02

import Data.Hashable
import Data.HashMap.Strict

f :: (Eq a, Hashable a) => [a] -> HashMap a Int
f xs = fromListWith (+) [(x,1)|x<-xs]



