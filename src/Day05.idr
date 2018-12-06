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

reduce : Polymer -> Polymer
reduce = foldr add []

remove_and_reduce : Char -> Polymer -> Polymer
remove_and_reduce c = reduce . filter (\x => toLower x /= c)

solve1 : String -> Nat
solve1 = length . reduce . unpack

solve2 : String -> Nat
solve2 = fromMaybe 0 . listToMaybe . sort . map (length . uncurry remove_and_reduce) . zip ['a' .. 'z'] . replicate 26 . unpack

partial day05 : IO ()
day05 = do
  putStr "Day 05: "
  assert 10 (solve1 "dabAcCaCBAcCcaDA")
  assert 4 (solve2 "dabAcCaCBAcCcaDA")
  string <- fromMaybe "" . listToMaybe <$> readFile' "input/day05.txt"
  print (solve1 string)
  putStr " | "
  print (solve2 string)
  putStrLn ""
