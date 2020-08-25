-- Part 1 --

fun1 :: [Integer] -> Integer
fun1 = foldr (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = foldr (+) 0 . filter even . collatz

collatz :: Integer -> [Integer]
collatz = takeWhile (/= 1) . iterate f
    where f x = if even x then div x 2 else 3 * x + 1

-- Part 2 --

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

addNode ::  Tree a -> Tree a -> Tree a
addNode node (Node d n1@(Node d1 cn1 v1 cn2) v n2@(Node d2 a v2 b))
    | d1 > d2   = Node d n1 v (addNode node n2)
    | d2 > d1   = Node d (addNode node n1) v n2
    | d2 == d1  = Node newD subtree v n2
        where 
            subtree = addNode node n1
            newD = depth subtree + 1 
addNode node (Node d Leaf c Leaf)     = Node (d + 1) (addNode node Leaf) c Leaf
addNode node (Node d l@(Leaf) c n2)   = Node d (addNode node l) c n2
addNode node (Node d n1 c l@(Leaf))   = Node d n1 c (addNode node l)
addNode node Leaf                     = node


depth :: Tree a -> Integer
depth (Node d _ _ _) = d


toNode :: a -> Tree a
toNode val = Node 0 Leaf val Leaf


foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf . map toNode 


-- Part 3 -- 

