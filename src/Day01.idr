import Data.SortedSet
import Data.String
import Effects
import Effect.Exception

%default total
%access public export

data InputError = EmptyInput

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

readFile' : (filepath : String) -> IO (List String)
readFile' fp = do
  eitherFile <- readFile fp
  case eitherFile of
    Left e => do print e; pure empty
    Right s => pure (lines s)

assert : (expected : Int) -> (actual : Int) -> IO ()
assert expected actual =
  if (actual /= expected)
    then putStrLn ("assertion failed: expectes " ++ show expected ++ ", but was " ++ show actual)
    else pure ()

on_freqs_eff : (Int -> IO ()) -> Eff Int [EXCEPTION InputError] -> IO ()
on_freqs_eff f eff =
  case the (Either InputError Int) (run eff) of
    Left EmptyInput => putStrLn "invalid input: list of freqs is empty"
    Right r         => f r

assert_eff : Int -> Eff Int [EXCEPTION InputError] -> IO ()
assert_eff = on_freqs_eff . assert

print_eff : Eff Int [EXCEPTION InputError] -> IO ()
print_eff = on_freqs_eff printLn

partial day01 : IO ()
day01 = do
  putStrLn "Day 01"
  assert 3 (solve1 [1, 1, 1])
  assert 0 (solve1 [1, 1, (-2)])
  assert (-6) (solve1 [(-1), (-2), (-3)])
  assert_eff 2 (solve2 [1, (-2), 3, 1])
  assert_eff 0 (solve2 [1, (-1)])
  assert_eff 10 (solve2 [3, 3, 4, (-2), (-4)])
  assert_eff 5 (solve2 [(-6), 3, 8, 5, (-6)])
  assert_eff 14 (solve2 [7, 7, (-2), (-7), (-4)])
  ints <- (catMaybes . map parseInteger) <$> readFile' "input/day01.txt"
  printLn (solve1 ints)
  print_eff (solve2 ints)
