module Project1 where
{-

Antonino DiMaggio, ajd311
Passed 24 of 24 provided test cases

-}
-- triples returns the number of items in a list that are divisible by 3.
--
-- > triples [1,2,3,4,5,6]
-- 2
-- > triples [3,33,333]
-- 3
-- > triples [0,1,4]
-- 1

{- Passed all 4 test cases -}
triples :: [Integer] -> Integer
triples [] = 0
triples (x:xs) = if mod x 3 == 0 then 1 + triples xs else triples xs


-- The hailstone sequence takes a positive number n and repeatedly applies
-- this transformation: if n is even, it divides n by 2; otherwise, it
-- multiplies n by 3 and adds one. The sequence ends when it reaches 1.
--
-- hailstone returns the complete sequence beginning with a particular number.
-- You may assume that the number is positive.
--
-- > hailstone 4
-- [4,2,1]
-- > hailstone 6
-- [6,3,10,5,16,8,4,2,1]
-- > hailstone 7
-- [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]

{- Passed all 4 test cases -}
hailstone :: Integer -> [Integer]
hailstone n =
  if n == 1 then n : []
  else if n `mod` 2 == 0 then n : hailstone (n `div` 2)
  else n : hailstone (3 * n + 1)

data Point = Pt Double Double deriving (Show, Eq)

-- The centroid of a list of points is a point whose x (and y) coordinates are
-- the means of the x (and y) coordinates of the points in the list.
--
-- You may assume the list contains at least one point.
--
-- > centroid [Pt 1 1, Pt 2 2]
-- Pt 1.5 1.5
-- > centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1]
-- Pt 0.5 1.0

getX :: Point -> Double
getX (Pt x y) = x

getY :: Point -> Double
getY (Pt x y) = y

sumX :: [Point] -> Double
sumX [] = 0
sumX(x:xs) = getX x + sumX xs

sumY :: [Point] -> Double
sumY [] = 0
sumY(y:ys) = getY y + sumY ys

setPt :: Double -> Double -> Point
setPt x y = Pt x y

{- Passed all 4 test cases -}
centroid :: [Point] -> Point
-- if empty list is passed
centroid (x:xs) = setPt (sumX (x:xs)/fromIntegral (length (x:xs))) (sumY (x:xs)/ fromIntegral (length (x:xs)))

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)


-- mirror returns a tree with the same shape and contents as its argument, but
-- flipped left for right.
--
-- > mirror (Bin (Bin Tip 1 (Bin Tip 2 Tip)) 3 Tip)
-- Bin Tip 3 (Bin (Bin Tip 2 Tip) 1 Tip)

{- Passed all 4 test cases -}
mirror :: Tree a -> Tree a
mirror Tip = Tip
mirror (Bin l x r)  = Bin (mirror r) x (mirror l)

-- In a strictly binary tree, each node has either 0 children or 2 children.
--
-- > strict (Bin Tip 1 Tip)
-- True
-- > strict (Bin Tip 1 (Bin Tip 2 Tip))
-- False

doubleCheck :: Tree a -> Tree b -> Bool

-- pattern matching cases
doubleCheck (Tip) (Tip) = True -- node has zero children
doubleCheck (Tip) (Bin _ _ _) = False -- node hase one child
doubleCheck (Bin _ _ _) (Tip) = False -- node has one child
doubleCheck (Bin _ _ _) (Bin _ _ _) = True -- node has two children that may be tips or may be binary trees


strict :: Tree a -> Bool
strict Tip = True
strict (Bin l _ r) =
  if (doubleCheck l r) == False then False
  else if (strict l) == False then False
  else if (strict r) == False then False
  else True


-- A tree is near balanced if the left and right sub-trees of each node differ
-- in height by at most 1.
--
-- > near_balanced (Bin Tip 1 (Bin Tip 2 Tip))
-- True
-- > near_balanced (Bin Tip 1 (Bin Tip 2 (Bin Tip 3 Tip)))
-- False
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin True 3 Tip)))
-- True
-- > near_balanced (Bin (Bin Tip 2 Tip) 1 (Bin Tip 2 (Bin (Bin Tip 4 Tip) 3 Tip)))
-- False

recurseMax :: Tree a -> Integer
recurseMax Tip = 0
recurseMax (Bin l _ r) = (max (1 + recurseMax l) (1 + recurseMax r))

recurseMin :: Tree a -> Integer
recurseMin Tip = 0
recurseMin (Bin l _ r) = (min (1 + recurseMin l) (1 + recurseMin r))

near_balanced :: Tree a -> Bool
near_balanced (Tip) = True
near_balanced (Bin l x r)
  |(abs (max (recurseMax l) (recurseMax r)) - (min (recurseMin r) (recurseMin l)) ) <= 1 = True
  | otherwise = False
