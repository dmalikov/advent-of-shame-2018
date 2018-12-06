module Core

import Effects
import Effect.Exception
import Lightyear
import Lightyear.Char
import Lightyear.Strings

%default total
%access public export


data InputError
  = EmptyInput
  | BadFormat String

assert : (Eq a, Show a) => (expected : a) -> (actual : a) -> IO ()
assert expected actual =
  if (actual /= expected)
    then putStrLn ("assertion failed: expectes " ++ show expected ++ ", but was " ++ show actual)
    else pure ()

on_ne_eff : (Eq a, Show a) => (a -> IO ()) -> Eff a [EXCEPTION InputError] -> IO ()
on_ne_eff f eff =
  case run eff of
    Left EmptyInput    => putStrLn "invalid input: empty"
    Left (BadFormat s) => putStrLn ("invalid input: bad format - " ++ s)
    Right r            => f r

assert_eff : (Eq a, Show a) => a -> Eff a [EXCEPTION InputError] -> IO ()
assert_eff = on_ne_eff . assert

print_eff : (Eq a, Show a) => Eff a [EXCEPTION InputError] -> IO ()
print_eff = on_ne_eff print

readFile' : (filepath : String) -> IO (List String)
readFile' fp = do
  eitherFile <- readFile fp
  case eitherFile of
    Left e => do print e; pure empty
    Right s => pure (lines s)

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

parse' : Parser a -> String -> Maybe a
parse' p i =
  case parse p i of
    Left e  => Nothing
    Right r => Just r

fromDigits : List (Fin 10) -> Integer
fromDigits = foldl (\a, b => 10 * a + cast b) 0

on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f x y = g (f x) (f y)
