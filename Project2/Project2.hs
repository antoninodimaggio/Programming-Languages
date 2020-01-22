module Project2 where

  {-

  Antonino DiMaggio, ajd311
  Passed 20 of 20 provided test cases

  -}


-- Modify this file by replacing the (non-)definitions of each function with
-- working definitions. Do not modify the types of the functions, as this may
-- prevent the grading script from compiling. Similarly, do not modify the
-- definitions of Tree or BTree or the constant xyz_code.
--
-- Note that the examples given here are illustrative but not exhaustive. You
-- must consider the definition and ensure that your functions are correct for
-- all reasonable inputs. (For example, your implementations of codes and decode
-- may assume that the Huffman tree given contains at least two symbols, each of
-- which occurs exactly once in the tree.)

data Tree a = Tip | Bin (Tree a) a (Tree a) deriving (Show, Eq)


-- (1) [10 points]
-- Return a list of every path from a root to a leaf in the tree, in order
-- from left to right. Nodes with one child (one Tip and one Bin) are not
-- considered leaf nodes.
--
-- > paths Tip
-- []
-- > paths (Bin Tip 'a' Tip)
-- ["a"]
-- > paths (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' (Bin Tip 'd' Tip)))
-- > ["ba", "bcd"]

paths :: Tree a -> [[a]]
paths Tip = []
paths (Bin l x r) = iter (Bin l x r) [] []
      where
      iter Tip ys xs = xs
      iter (Bin Tip x Tip) ys xs = (ys ++ [x]) : xs
      iter (Bin l x r) ys xs = (iter l (ys ++ [x]) xs) ++ (iter r (ys ++ [x]) xs)


-- (2) [10 points]
-- Return a tree with the same shape as its argument, but with each node label
-- replaced with a pair containing the original label in the second field.
-- The first field should be the height of the subtree rooted at that node.
--
-- heights Tip
-- > Tip
-- heights (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' (Bin Tip 'd' Tip)))
-- > Bin (Bin Tip (1,'a') Tip) (3,'b') (Bin Tip (2,'c') (Bin Tip (1,'d') Tip))

heights :: Tree a -> Tree (Integer, a)
heights Tip = Tip
heights (Bin l x r) = Bin (heights l) (max (findSubHeight l) (findSubHeight r), x) (heights r)

findSubHeight :: Tree a -> Integer
findSubHeight Tip = 1
findSubHeight (Bin l x r) = (max (1 + findSubHeight l) (1 + findSubHeight r))


-- Huffman coding
-- --------------
--
-- A Huffman tree encodes symbols into bits using a binary decision tree. We
-- will use the BTree type defined below to represent the decision tree, where
-- the left branch corresponds to a 0 and the right to 1. The Leaf nodes contain
-- the symbol encoded by the path leading to that leaf.

data BTree a = Leaf a | Fork (BTree a) (BTree a) deriving (Show, Eq)

-- Here is a sample tree giving encodings for three Char values: 'x' is 0,
-- 'y' is 10 and 'z' is 11.

xyz_code :: BTree Char
xyz_code =
    Fork
        (Leaf 'x')
        (Fork
            (Leaf 'y')
            (Leaf 'z'))

-- collapse (added) xyz_code = (Fork (Leaf 'x') (Fork (Leaf 'y')(Leaf 'z')))

-- In the assignments below, we will use [Bool] to represent a bit sequence.
-- This function turns [Bool] into String for ease of interpretation.

bitStr :: [Bool] -> String
bitStr = map (\b -> if b then '1' else '0')


-- (3) [8 points]
-- (The order of symbols is not
-- significant.)
--
-- > codes xyz_code
-- [('x',[False]),('y',[True,False]),('z',[True,True])]

codes :: BTree a -> [(a, [Bool])]
codes (Fork l r) = looper (Fork l r) [] []
      where
      looper (Leaf x) ys xs = (x, ys) : xs
      looper (Fork l r) ys xs = (looper l (ys ++ [False]) xs) ++ (looper r (ys ++ [True]) xs)


-- (4) [12 points]
-- Given a Huffman tree and a stream of bits, return a pair containing (1) the
-- string of symbols encoded by the bits (according to the Huffman tree), and
-- (2) a Bool indicating whether the output stream contains every bit from the
-- input (that is, return False if there were any bits left over).
--
-- > decode xyz_code [True, False]
-- ("y",True)
-- > decode xyz_code [True,False,True,True,True,False,False,True,False]
-- ("yzyxy",True)
-- > decode xyz_code [True,False,True,True,True,False,False,True]
-- ("yzyx",False)

decode :: BTree a -> [Bool] -> ([a], Bool)
decode (Fork l r) (b:bs) = loop (Fork l r) (b:bs) []
      where
      loop (Leaf x) [] xs = (xs ++ [x], True)
      loop (Leaf x) (b:bs) xs = if b == True then (loop r bs (xs ++ [x])) else (loop l bs (xs ++ [x]))
      loop (Fork l r) (b:bs) xs = if b == True then (loop r bs xs) else (loop l bs xs)
      loop _ _ xs = (xs, False)


-- (5)
-- For extra credit, make your implementation of decode lazy, so that the
-- input stream is consumed as symbols from the output are demanded.
--

-- > take 3 . fst . decode xyz_code $ repeat True
-- "zzz"

-- I think this should only return "z"
-- The issue with this is that we will never get to the take because we are still evaluating decode due to the repeat

-- > take 3 . fst . decode xyz_code $ False:False:False:error "too far"
-- "xxx"
--
-- To be tested for extra credit, set the following Bool to True.

extra_credit = False
