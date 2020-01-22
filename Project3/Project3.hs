module Project3 where

{-
Antonino DiMaggio
passed all 50 test cases
-}

-- Modify this file by replacing the (non-)definitions of each regular
-- expression and function with working definitions. Do not modify the types of
-- the expressions or functions, as this may prevent the grading script from
-- compiling. Similarly, do not modify the definitions of RE or ABC.


import Data.List

data RE sym
    = Never             -- match no strings
    | Empty             -- match empty string
    | Symbol sym        -- match singleton string
    | RE sym :+ RE sym  -- choice
    | RE sym :* RE sym  -- concatenation
    | Repeat (RE sym)   -- repeat zero or more times
    | Plus (RE sym)     -- repeat one or more times
    deriving (Show, Eq)

infixr 6 :+, .+.
infixr 7 :*, .*.

data ABC = A | B | C deriving (Show, Eq, Ord)


-- 1
-- Create a regular expression for the language over ABC containing strings
-- with exactly one C

-- write this out

one :: RE ABC
one = Repeat (Repeat (Symbol A) :* Repeat (Symbol B)) :* Symbol C :* Repeat (Repeat (Symbol A) :* Repeat (Symbol B))


-- 2
-- Create a regular expression for the language over ABC containing strings
-- with an even number of A's

-- 0 is an even number
-- write this out

two :: RE ABC
two = Repeat ((Repeat (Repeat (Symbol B) :* Repeat (Symbol C))) :* Symbol A :* (Repeat (Repeat (Symbol B) :* Repeat (Symbol C))) :* Symbol A :* (Repeat (Repeat (Symbol B) :* Repeat (Symbol C)))) :+ Repeat (Repeat (Symbol B) :* Repeat (Symbol C))


-- 3
-- Create a regular expression for the language over ABC containing strings
-- where every A is immediately followed by a B
-- (ab|b|c)*

three :: RE ABC
three = Repeat ((Symbol A :* Symbol B) :+ (Symbol B) :+ (Symbol C))
-- three =  Repeat ((Repeat (Repeat (Symbol B) :* Repeat (Symbol C))) :+ (Symbol A :* Symbol B)  :+ (Repeat (Repeat (Symbol B) :* Repeat (Symbol C))))


-- 4
-- Write a function matchEmpty that returns true for regular expressions that
-- match the empty string.
-- matchEmpty ((Repeat r) :* s) = matchEmpty s {wrong}

matchEmpty :: RE sym -> Bool

matchEmpty Never = False
matchEmpty Empty = True
matchEmpty (Symbol x) = False
matchEmpty (Repeat _) = True
matchEmpty (Plus r) = matchEmpty r
matchEmpty (r:+s) = matchEmpty r || matchEmpty s
matchEmpty (r:*s) = matchEmpty r && matchEmpty s
matchEmpty _ = False


-- 5
-- Write a function firsts that, for a regular expression p, returns a list of
-- symbols such that (1) every string matching p begins with a symbol that
-- occurs somewhere in firsts p, and (2) every symbol occuring in firsts p
-- appears at the beginning of some string in the language of p.
--
-- Note that the symbol type is completely polymorphic, so it is not possible
-- to sort the list or remove duplicates. Note also that the list must be
-- finite, even if the number of strings in the language is infinite.


firsts :: RE sym -> [sym]
firsts Never = []
firsts Empty = []
firsts (Symbol a) = [a]
firsts ((Repeat r) :* s) = firsts r ++ firsts s
firsts (Repeat r) = firsts r
firsts (r:+s) = firsts r ++ firsts s
firsts (r:*s) = firsts r
firsts (Plus r) = firsts r


-- utilities
-- You may use matchDeriv to check your regular expressions. Note that you
-- must define matchEmpty in order for matchDeriv to work.

matchDeriv :: (Eq sym) => RE sym -> [sym] -> Bool
matchDeriv p = matchEmpty . foldl' deriv p

deriv :: (Eq sym) => RE sym -> sym -> RE sym
deriv Never      _ = Never
deriv Empty      _ = Never
deriv (Symbol s) x
    | s == x       = Empty
    | otherwise    = Never
deriv (p :+ q)   x = deriv p x .+. deriv q x
deriv (p :* q) x
    | matchEmpty p = deriv q x .+. deriv p x .*. q
    | otherwise    = deriv p x .*. q
deriv (Repeat p) x = deriv p x .*. Repeat p
deriv (Plus p)   x = deriv p x .*. Repeat p

-- Alternative forms of :+ and :* that perform some elementary simplification.
-- These reduce the size of the expressions deriv produces in many common cases.

(.+.) :: RE sym -> RE sym -> RE sym
Never .+. q     = q
p     .+. Never = p
p     .+. q     = p :+ q

(.*.) :: RE sym -> RE sym -> RE sym
Never .*. q = Never
Empty .*. q = q
p     .*. q = p :* q
