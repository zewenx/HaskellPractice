-- @Author: anchen
-- @Date:   2017-08-25 12:28:42
-- @Last Modified by:   anchen
-- @Last Modified time: 2017-08-25 14:41:29

-- work3 --
-- Q2
ftoc :: Fractional a => a -> a
ftoc f = (5/9)*(f-32);

--Q3
--quadRoots :: Double -> Double -> Double -> [Double]
quadRoots :: (Floating c, Ord c) => c -> c -> c -> [c]
quadRoots a b c 
    | a == 0 && b ==0 = error "Either a or b must be non-zero"
    | a == 0 =[-c/b]
    | disc < 0 = error "No real solutions"
    | disc == 0 = [tp,tp]
    | disc > 0 =[tp + temp,tp-temp]
    where disc = b*b - 4*a*c
          temp = sqrt(disc) / (2*a)
          tp   = -b / (2*a)


-- Q6
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
same_shape :: Tree a b -> Tree c d -> Bool
same_shape Leaf Leaf = True
same_shape Leaf (Node _ _ _ _) = False
same_shape (Node _ _ _ _) Leaf = False
same_shape (Node _ _ l1 r1) (Node _ _ l2 r2) = (same_shape l1 l2) && (same_shape r1 r2)

tree1 = Node 1 1 (Node 2 2 Leaf (Node 3 4 Leaf Leaf)) (Node 4 4 (Node 5 5 Leaf Leaf)(Node 6 6 (Node 7 7 Leaf Leaf) Leaf))

tree2 = Node 1 1 (Node 2 2 Leaf (Node 3 4 Leaf Leaf)) (Node 4 4 (Node 5 5 Leaf Leaf)(Node 6 6 Leaf Leaf))

-- Q7
data Expression 
    = Var Variable 
    | Num Integer 
    | Plus Expression Expression 
    | Minus Expression Expression
    | Times Expression Expression
    | Div Expression Expression
data Variable = A | B

exp1 = Plus (Times (Num 2) (Var A)) (Var B)
exp2 = Div (Plus (Times (Var A) (Var A)) (Times (Var B) (Var B))) (Times (Var A)(Var B)) 
eval :: Integer -> Integer -> Expression -> Integer
eval a b exp =
    case exp of Plus l r -> (eval a b l) + (eval a b r)
                Minus l r -> (eval a b l) - (eval a b r)
                Times l r -> (eval a b l) * (eval a b r)
                Div l r -> (eval a b l) `div` (eval a b r)
                Var A -> a
                Var B -> b
                Num c->c

-- workshop 4
transcope :: [[Int]] -> [[Int]]
transcope (a:as):ss = [(a:transcope ss),transcope as]