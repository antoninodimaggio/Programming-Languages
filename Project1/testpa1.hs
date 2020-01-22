{-# LANGUAGE BangPatterns #-}
module Main where

-- Do not post this code anywhere on-line or share with students outside this
-- class.
--
-- Note that this test code uses the QuickCheck package, which is not always
-- installed by default.
-- run this: stack ghci --package QuickCheck -- testpa1.hs

import System.Timeout
import Test.QuickCheck
import Control.Applicative
import Control.Exception (catch, evaluate, SomeException)
import Project1 (Point(..), Tree(..))
import qualified Project1 as P

-- ensure correct types

triples, ref_triples :: [Integer] -> Integer
triples = P.triples

hailstone, ref_hailstone :: Integer -> [Integer]
hailstone = P.hailstone

centroid, ref_centroid :: [Point] -> Point
centroid = P.centroid

mirror, ref_mirror :: Tree a -> Tree a
mirror = P.mirror

strict, ref_strict :: Tree a -> Bool
strict = P.strict

near_balanced, ref_near_balanced :: Tree a -> Bool
near_balanced = P.near_balanced


-- reference implementations

ref_triples = fromIntegral . length . filter (\x -> x `mod` 3 == 0)

prop_triples ints = triples ints == ref_triples ints

ref_hailstone n
    | n == 1 = [1]
    | even n = n : ref_hailstone (n `div` 2)
    | otherwise = n : ref_hailstone (n*3+1)

prop_hailstone (Positive n) = hailstone n == ref_hailstone n

ref_centroid = go 0 0 0
    where
    go !l !sx !sy [] = Pt (sx / l) (sy / l)
    go !l !sx !sy (Pt x y : ps) = go (l + 1) (sx + x) (sy + y) ps

prop_centroid (NonEmpty pts)
    | mag == 0  = diff < 1e-5
    | otherwise = diff / mag < 1e-5
    where
    Pt x y = centroid pts
    Pt u v = ref_centroid pts
    diff = (x-u)^2 + (y-v)^2
    mag = u^2 + v^2

ref_mirror Tip = Tip
ref_mirror (Bin l x r) = Bin (mirror r) x (mirror l)

prop_mirror, prop_strict, prop_near_balanced :: Tree Char -> Bool
prop_mirror t = mirror t == ref_mirror t

ref_strict Tip = True
ref_strict (Bin l x r) = go l r
    where
    go Tip Tip = True
    go (Bin ll _ lr) (Bin rl _ rr) = go ll lr && go rl rr
    go _ _ = False

prop_strict t = strict t == ref_strict t

ref_near_balanced = fst . nb
    where
    nb Tip = (True, 0)
    nb (Bin l _ r) = (bl && br && abs (hl - hr) < 2, 1 + max hl hr)
        where
        (bl, hl) = nb l
        (br, hr) = nb r

prop_near_balanced t = near_balanced t == ref_near_balanced t

-- QuickCheck support

instance Arbitrary Point where
    arbitrary = Pt <$> arbitrary <*> arbitrary

    shrink (Pt x y)
        =  [ Pt sx y | sx <- shrink x ]
        ++ [ Pt x sy | sy <- shrink y ]

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized (arbTree arbitrary)

    shrink Tip = []
    shrink (Bin l x r)
        =  [Tip]
        ++ [ Bin sl x r | sl <- shrink l ]
        ++ [ Bin l x sr | sr <- shrink r ]
        ++ [ Bin l sx r | sx <- shrink x ]

arbTree item n
    | n < 1 = pure Tip
    | otherwise = oneof
        [ pure Tip
        , Bin <$> arbTree item h <*> item <*> arbTree item h
        ]
        where h = n `div` 2

-- Test cases

tests =
    [ ( "triples"
      , [ test_triples [1,2,3,4,5,6] 2
        , test_triples [3,33,333] 3
        , test_triples [0,1,4] 1
        , test_triples [] 0
        ]
      , property prop_triples
      )
    , ( "hailstone"
      , [ test_hailstone 4 [4,2,1]
        , test_hailstone 6 [6,3,10,5,16,8,4,2,1]
        , test_hailstone 7 [7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
        , test_hailstone 1 [1]
        ]
      , property prop_hailstone
      )
    , ( "centroid"
      , [ test_centroid [Pt 1 1, Pt 2 2] (Pt 1.5 1.5)
        , test_centroid [Pt (-1.5) 0, Pt 3 2, Pt 0 1] (Pt 0.5 1.0)
        , test_centroid [Pt 8 6, Pt 5.5 4, Pt 100 0, Pt (-29) 17] (Pt 21.125 6.75)
        , test_centroid [Pt 10 10] (Pt 10 10)
        ]
      , property prop_centroid
      )
    , ( "mirror"
      , [ test_mirror (Bin Tip 1 (Bin Tip 2 Tip)) (Bin (Bin Tip 2 Tip) 1 Tip)
        , test_mirror Tip (Tip :: Tree ())
        , test_mirror (Bin Tip 1 Tip) (Bin Tip 1 Tip)
        , test_mirror (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' Tip))
                      (Bin (Bin Tip 'c' Tip) 'b' (Bin Tip 'a' Tip))
        ]
      , property prop_mirror
      )
    , ( "strict"
      , [ test_strict (Bin Tip 1 Tip) True
        , test_strict (Bin Tip 1 (Bin Tip 2 Tip)) False
        , test_strict (Tip :: Tree ()) True
        , test_strict (Bin (Bin (Bin Tip 'a' Tip) 'b' (Bin Tip 'c' Tip)) 'd' (Bin Tip 'e' Tip)) True
        ]
      , property prop_strict
      )
    , ( "near_balanced"
      , [ test_near_balanced (Bin Tip 1 Tip) True
        , test_near_balanced (Bin (Bin Tip 1 Tip) 1 Tip) True
        , test_near_balanced
            (Bin (Bin Tip 2 Tip) 1 (Bin (Bin Tip 3 Tip) 2 Tip)) True
        , test_near_balanced
            (Bin (Bin Tip 2 Tip) 1 (Bin (Bin Tip 3 (Bin Tip 4 Tip)) 2 Tip)) False
        ]
      , property prop_near_balanced
      )
    ]



timelimit = 1000000

testBy :: (Show a) => (a -> a -> Bool) -> String -> a -> a -> IO Bool
testBy eq name got want = catch body handle
    where
    body = do
        -- putStrLn $ "testing " ++ name
        ok <- timeout timelimit (evaluate (eq got want))
        case ok of
            Just True -> return True
            Just False -> fail "INCORRECT" (show got)
            Nothing    -> fail "TIMEOUT" ""

    handle :: SomeException -> IO Bool
    handle e = fail "ERROR" (show e)

    fail msg txt = do
        putStrLn $ msg ++ ": " ++ name
        putStrLn $ "       wanted: " ++ show want
        putStrLn $ "       got:    " ++ take 200 txt
        putStrLn ""
        return False


test :: (Eq a, Show a) => String -> a -> a -> IO Bool
test = testBy (==)

test_f1 :: (Eq b, Show a, Show b) => String -> (a -> b) -> a -> b -> IO Bool
test_f1 name f x r = test fx (f x) r
    where fx = (showString name . showChar ' ' . showsPrec 11 x) ""

test_triples       = test_f1 "triples" triples
test_hailstone     = test_f1 "hailstone" hailstone
test_centroid      = test_f1 "centroid" centroid

test_mirror :: (Eq a, Show a) => Tree a -> Tree a -> IO Bool
test_mirror        = test_f1 "mirror" mirror

test_strict :: (Eq a, Show a) => Tree a -> Bool -> IO Bool
test_strict        = test_f1 "strict" strict

test_near_balanced :: (Eq a, Show a) => Tree a -> Bool -> IO Bool
test_near_balanced = test_f1 "near_balanced" near_balanced


runTests score [] = return score
runTests score (t:ts) = do
    b <- t
    runTests (if b then score + 1 else score) ts

args = stdArgs { maxSuccess = 500 }

runTestGroups !score [] = return score
runTestGroups !score ((name, tests, prop) : groups) = do
    putStrLn $ "\nTesting " ++ name
    tscore <- runTests 0 tests
    putStrLn $ "\nUnit tests passed: " ++ show tscore
    r <- quickCheckWithResult args (within timelimit prop)
    pscore <- case r of
        Success{} -> return 4
        _ ->         return 0
    runTestGroups (score + tscore + pscore) groups

main = do
    score <- runTestGroups 0 tests
    putStrLn $ "Score: " ++ show score
