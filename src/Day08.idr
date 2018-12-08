module Day08

import Data.Vect as V
import Lightyear
import Lightyear.Char
import Lightyear.Combinators
import Lightyear.Strings

import Core

%default total -- TODO: how to tell Idris that all the Tree manipulations are actuall total?
%access public export


Metadata : Type
Metadata = List Integer

data Tree = MkTree (List Tree) Metadata

partial Show Tree where
  show (MkTree nodes meta) = "< " ++ show nodes ++ " { " ++ show meta ++ " } ]"

partial metadata_sum : Tree -> Integer
metadata_sum (MkTree nodes meta) = sum meta + sum (metadata_sum <$> nodes)

partial natP : Parser Nat
natP = mult <$> some digit
  where
    mult : List (Fin 10) -> Nat
    mult = foldl (\a, b => 10 * a + cast b) 0

partial treeP : Parser Tree
treeP = do
  node_cnt <- natP
  meta_length <- space *> natP
  nodes <- toList <$> ntimes node_cnt (space *> treeP)
  meta <- toList <$> ntimes meta_length (fromDigits <$> (space *> some digit))
  pure (MkTree nodes meta)

partial solve1 : String -> Integer
solve1 = fromMaybe 0 . map metadata_sum . parse' treeP

partial day08 : IO ()
day08 = do
  putStr "Day08: "
  assert 138 (solve1 "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
  string <- fromMaybe "" . listToMaybe <$> readFile' "input/day08.txt"
  print (solve1 string)
  putStr " | "
  --print (solve2 string)
  putStrLn ""
