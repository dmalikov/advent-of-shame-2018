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

assert : Int -> Int -> IO ()
assert actual expected =
  if (actual /= expected)
    then putStrLn ("assertion failed: expectes " ++ show expected ++ ", but was " ++ show actual)
    else pure ()

assert_eff : Eff Int [EXCEPTION InputError] -> Int -> IO ()
assert_eff actual_eff expected =
  case the (Either InputError Int) (run actual_eff) of
    Left EmptyInput => putStrLn "invalid input: list of freqs is empty"
    Right actual    => assert actual expected

print_eff : Eff Int [EXCEPTION InputError] -> IO ()
print_eff eff = do
  case the (Either InputError Int) (run eff) of
       Left EmptyInput => putStrLn "invalid input: list of freqs is empty"
       Right actual    => printLn actual

partial day01 : IO ()
day01 = do
  assert (solve1 [1, 1, 1]) 3
  assert (solve1 [1, 1, (-2)]) 0
  assert (solve1 [(-1), (-2), (-3)]) (-6)
  assert_eff (solve2 [1, (-2), 3, 1]) 2
  assert_eff (solve2 [1, (-1)]) 0
  assert_eff (solve2 [3, 3, 4, (-2), (-4)]) 10
  assert_eff (solve2 [(-6), 3, 8, 5, (-6)]) 5
  assert_eff (solve2 [7, 7, (-2), (-7), (-4)]) 14
  ints <- (catMaybes . map parseInteger) <$> readFile' "input/day01.txt"
  printLn (solve1 ints)
  print_eff (solve2 ints)
