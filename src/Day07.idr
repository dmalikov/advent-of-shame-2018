module Day07

import Data.List
import Data.SortedMap as SM
import Data.SortedSet as SS
import Data.Vect
import Lightyear
import Lightyear.Char
import Lightyear.Combinators
import Lightyear.Strings
-- TODO: can redundant imports be detected?

import Core

%default total
%access public export


Step : Type
Step = Char

data StepDep = MkStepDep Step Step

Show StepDep where
  show (MkStepDep a b) = (cast a) ++ " -> " ++ (cast b)

Sleigh : Type
Sleigh = List Step

StepsCodeps : Type
StepsCodeps = SortedMap Step (SortedSet Step)

Show StepsCodeps where
  show = unlines . map (\(k, vs) => (show k) ++ ": " ++ show (SS.toList vs)) . SM.toList

mkStepsCodeps : List StepDep -> StepsCodeps
mkStepsCodeps = foldl ins SM.empty
  where
    ins : StepsCodeps -> StepDep -> StepsCodeps
    ins xs' (MkStepDep f t) = (ins_f . ins_t) $ xs'
      where
        ins_t : StepsCodeps -> StepsCodeps
        ins_t xs =
          case lookup t xs of
            Nothing => SM.insert t (SS.fromList [f]) xs
            Just f' => SM.insert t (SS.insert f f') xs
        ins_f : StepsCodeps -> StepsCodeps
        ins_f xs =
          case lookup f xs of
            Nothing => SM.insert f SS.empty xs
            Just _ => xs

data State = MkState StepsCodeps Sleigh

available : StepsCodeps -> Maybe Step
available = listToMaybe . map fst . filter ((== []) . SS.toList . snd) . toList

step : Step -> StepsCodeps -> StepsCodeps
step x = delete x . map (delete x)

partial eval : StepsCodeps -> Sleigh
eval codes =
  case available codes of
    Nothing => []
    Just s => s :: (eval (step s codes))

stepDepP : Parser StepDep
stepDepP = MkStepDep
  <$> (string "Step " *> letter)
  <*> (string " must be finished before step " *> letter <* string " can begin.")

partial solve1 : List String -> String
solve1 = pack . eval . mkStepsCodeps . catMaybes . map (parse' stepDepP)

test_input : List String
test_input =
  [ "Step C must be finished before step A can begin."
  , "Step C must be finished before step F can begin."
  , "Step A must be finished before step B can begin."
  , "Step A must be finished before step D can begin."
  , "Step B must be finished before step E can begin."
  , "Step D must be finished before step E can begin."
  , "Step F must be finished before step E can begin."
  ]

partial day07 : IO ()
day07 = do
  putStr "Day 07: "
  assert "CABDFE" (solve1 test_input)
  strings <- readFile' "input/day07.txt"
  putStr (solve1 strings)
  putStr " | "
  -- print (solve2 strings)
  putStrLn ""
