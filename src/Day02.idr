module Day02

import Data.SortedSet
import Data.String
import Effects
import Effect.Exception

import Core

%default total
%access public export


Box : Type
Box = List Char

record Counter where
  constructor MkCounter
  hasTwo, hasThree : Bool

record AggCounter where
  constructor MkAggCounter
  twos, threes : Nat

checksum : AggCounter -> Integer
checksum (MkAggCounter twos threes) = (toIntegerNat twos) * (toIntegerNat threes)

agg_counter : List Counter -> AggCounter
agg_counter counters = MkAggCounter
  (length (filter hasTwo   counters))
  (length (filter hasThree counters))

group' : (Eq a) => (current : a) -> (cnt : Nat) -> List a -> List (a, Nat)
group' current cnt [] = [(current, cnt)]
group' current cnt (x :: xs) =
  if current == x
    then group' current (cnt + 1) xs
    else (current, cnt) :: group' x 1 xs

-- | Group identical elements.
-- ["a", "b", "a", "b", "b", "a"] -> [("a", 1), ("b", 1), ("a", 1), ("b", 2), ("a", 1)]
group : (Eq a) => List a -> List (a, Nat)
group [] = []
group (x :: xs) = group' x 1 xs

elem_counts : (Eq a, Ord a) => List a -> List Nat
elem_counts = map snd . group . sort

count : Box -> Counter
count letters = let letter_counts = elem_counts letters in
  MkCounter
    (elem 2 letter_counts)
    (elem 3 letter_counts)

solve1 : List String -> Integer
solve1 = checksum . agg_counter . map (count . unpack)

pairs : Eq a => List a -> List (a, a)
pairs xs = do
  (x, idx) <- zip xs [1 .. length xs]
  y <- drop idx xs
  pure (x, y)

diff : (Eq a) => List a -> List a -> Nat
diff [] _ = 0
diff (x :: xs) [] = 0
diff (x :: xs) (y :: ys) =
  if x /= y
    then 1 + (diff xs ys)
    else diff xs ys

diff_in_1 : (Eq a) => List a -> List a -> Bool
diff_in_1 xs ys = (diff xs ys) == 1

common_elems : (Eq a) => List a -> List a -> List a
common_elems [] _ = []
common_elems (x :: xs) [] = []
common_elems (x :: xs) (y :: ys) =
  if x == y
    then x :: (common_elems xs ys)
    else common_elems xs ys

maybeList : Maybe (List a) -> List a
maybeList Nothing   = []
maybeList (Just xs) = xs

solve2' : List Box -> List Char
solve2' = maybeList . map (uncurry common_elems) . listToMaybe . filter (uncurry diff_in_1) . pairs

solve2 : List String -> String
solve2 = pack . solve2' . map unpack

partial day02 : IO ()
day02 = do
  putStrLn "Day 02"
  assert 12 (solve1
    [ "abcdef"
    , "bababc"
    , "abbcde"
    , "abcccd"
    , "aabcdd"
    , "abcdee"
    , "ababab"
    ])
  assert "fgij" (solve2
    [ "abcde"
    , "fghij"
    , "klmno"
    , "pqrst"
    , "fguij"
    , "axcye"
    , "wvxyz"
    ])
  boxes <- readFile' "input/day02.txt"
  printLn (solve1 boxes)
  putStrLn (solve2 boxes)
