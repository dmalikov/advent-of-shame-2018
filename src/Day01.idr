module Day01

import Data.SortedSet
import Data.String
import Effects
import Effect.Exception

import Core

%default total
%access public export

solve1 : List Int -> Int
solve1 = foldl (\a,x => a + x) 0

partial solve2' : (current_freq : Int) -> (known_freqs : SortedSet Int) -> Stream Int -> Int
solve2' cf kfs (x :: xs) =
  if contains cf kfs
    then cf
    else solve2' (cf + x) (insert cf kfs) xs

partial solve2 : List Int -> Eff Int [EXCEPTION InputError]
solve2 [] = raise EmptyInput
solve2 (x :: xs) = pure (solve2' 0 empty (cycle (x :: xs)))

partial day01 : IO ()
day01 = do
  putStr "Day 01: "
  assert 3 (solve1 [1, 1, 1])
  assert 0 (solve1 [1, 1, (-2)])
  assert (-6) (solve1 [(-1), (-2), (-3)])
  assert_eff 2 (solve2 [1, (-2), 3, 1])
  assert_eff 0 (solve2 [1, (-1)])
  assert_eff 10 (solve2 [3, 3, 4, (-2), (-4)])
  assert_eff 5 (solve2 [(-6), 3, 8, 5, (-6)])
  assert_eff 14 (solve2 [7, 7, (-2), (-7), (-4)])
  ints <- (catMaybes . map parseInteger) <$> readFile' "input/day01.txt"
  print (solve1 ints)
  putStr " | "
  print_eff (solve2 ints)
  putStrLn ""
