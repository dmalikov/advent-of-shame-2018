module Core

import Effects
import Effect.Exception
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import public Data.Either.Contrib
import public Data.Function.Contrib
import public Data.List.Contrib

%default total
%access public export


data InputError
  = EmptyInput
  | BadFormat String

assert : (Eq a, Show a) => (expected : a) -> (actual : a) -> IO ()
assert expected actual =
  if actual /= expected
    then putStrLn ("assertion failed: expected " ++ show expected ++ ", but was " ++ show actual)
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
    Left e => do
      print e
      pure empty
    Right s => pure (lines s)

parse' : Parser a -> String -> Maybe a
parse' p = eitherToMaybe . parse p

fromDigits : List (Fin 10) -> Integer
fromDigits = foldl (\a, b => 10 * a + cast b) 0
