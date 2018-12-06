module Day05

import Core

%default total
%access public export



Polymer : Type
Polymer = List Char

-- | same type different polarity, like 'a' and 'A'
stdp : Char -> Char -> Bool
stdp x y = on (==) toLower x y && x /= y

add : Char -> Polymer -> Polymer
add x [] = [x]
add x (y :: xs) =
  if stdp x y
    then xs
    else x :: y :: xs

solve1 : String -> Nat
solve1 = length . foldr add [] . unpack

partial day05 : IO ()
day05 = do
  putStr "Day 05: "
  assert 10 (solve1 "dabAcCaCBAcCcaDA")
  string <- fromMaybe "" . listToMaybe <$> readFile' "input/day05.txt"
  print (solve1 string)
  putStrLn ""
