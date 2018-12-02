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
  boxes <- readFile' "input/day02.txt"
  printLn (solve1 boxes)
