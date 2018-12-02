module Core

import Effects
import Effect.Exception

%default total
%access public export


data InputError = EmptyInput

assert : (Eq a, Show a) => (expected : a) -> (actual : a) -> IO ()
assert expected actual =
  if (actual /= expected)
    then putStrLn ("assertion failed: expectes " ++ show expected ++ ", but was " ++ show actual)
    else pure ()

on_ne_eff : (Eq a, Show a) => (a -> IO ()) -> Eff a [EXCEPTION InputError] -> IO ()
on_ne_eff f eff =
  case run eff of
    Left EmptyInput => putStrLn "invalid input: input list is empty"
    Right r         => f r

assert_eff : (Eq a, Show a) => a -> Eff a [EXCEPTION InputError] -> IO ()
assert_eff = on_ne_eff . assert

print_eff : (Eq a, Show a) => Eff a [EXCEPTION InputError] -> IO ()
print_eff = on_ne_eff printLn

readFile' : (filepath : String) -> IO (List String)
readFile' fp = do
  eitherFile <- readFile fp
  case eitherFile of
    Left e => do print e; pure empty
    Right s => pure (lines s)
