module Day07

import Control.Arrow
import Data.List
import Data.SortedMap as SM
import Data.SortedSet as SS
import Data.Vect as V
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

available : StepsCodeps -> List Step
available = map fst . filter ((== []) . SS.toList . snd) . toList

available' : StepsCodeps -> Maybe Step
available' = listToMaybe . available

step : Step -> StepsCodeps -> StepsCodeps
step x = delete x . map (delete x)

step_many : List Step -> StepsCodeps -> StepsCodeps
step_many [] codeps = codeps
step_many (x :: xs) codeps = step_many xs (step x codeps)

stepDepP : Parser StepDep
stepDepP = MkStepDep
  <$> (string "Step " *> letter)
  <*> (string " must be finished before step " *> letter <* string " can begin.")

partial eval : StepsCodeps -> Sleigh
eval codes =
  case available' codes of
    Nothing => []
    Just s => s :: (eval (step s codes))

partial solve1 : List String -> String
solve1 = pack . eval . mkStepsCodeps . catMaybes . map (parse' stepDepP)

-- [ Part 2. Multiple workers ] --------------------------------------------------

-- Progress of the steps.
-- 1) Z as a values stands for the step that would be completed in the next evaluation
-- 2) Such a values are filtered before the evaluation
-- 3) During the evaluation all the values are decreased
StepsProgress : Type
StepsProgress = SortedMap Step Nat

-- How much seconds it would take to complete the step.
-- Note: because of the notion of StepsProgress, however 'A' takes d+1 seconds,
--   we encode it as d.
step_work : Nat -> Step -> Nat
step_work d s = cast (ord s - ord 'A') + d

record WState where
  constructor MkWState
  codeps : StepsCodeps
  workers : StepsProgress
  in_parallel : Nat -- TODO separate types for these nats
  sec : Nat
  delay : Nat

init_state : Nat -> Nat -> StepsCodeps -> WState
init_state n delay codeps = MkWState codeps empty n Z delay

-- Evaluation execute the second.
-- If there are available steps, they should be added to the progress
--   before calling this function.
eval_workers : StepsProgress
            -> (List Step, StepsProgress)
eval_workers progress = (finished, progress')
  where
    remove_completed : StepsProgress -> StepsProgress
    remove_completed = SM.fromList . (Prelude.List.filter ((/= Z) . snd)) . SM.toList

    eval_worker : Nat -> Nat
    eval_worker (S n) = n
    eval_worker Z = Z -- this is not going to happen, TODO: prove

    progress' : StepsProgress
    progress' = map eval_worker . remove_completed $ progress

    finished : List Step
    finished = SS.toList (SS.difference (SS.fromList (SM.keys progress))
                                        (SS.fromList (SM.keys progress')))

sched_work : Nat -> Nat -> List Step -> StepsProgress -> StepsProgress
sched_work delay in_parallel [] progress = progress
sched_work delay in_parallel (x :: xs) progress = sched_work delay in_parallel xs sched_for_available
  where
    sched_for_available : StepsProgress
    sched_for_available =
      if (length (keys progress)) < in_parallel && isNothing (lookup x progress)
        then insert x (step_work delay x) progress
        else progress

partial eval_state : WState -> Nat
eval_state state@(MkWState codeps workers in_parallel sec delay) =
  let av' = take in_parallel $ available codeps
      progress' = sched_work delay in_parallel av' workers
      (completed, progress'') = eval_workers progress'
      codeps' = step_many completed codeps
      sec' = S sec
   in
      if null codeps'
        then sec'
        else eval_state $ MkWState codeps' progress'' in_parallel sec' delay

partial solve2 : Nat -> Nat -> List String -> Nat
solve2 n delay = eval_state . init_state n delay . mkStepsCodeps . catMaybes . map (parse' stepDepP)

-- [ Main ] --------------------------------------------------

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
  assert 15 (solve2 2 0 test_input)
  strings <- readFile' "input/day07.txt"
  putStr (solve1 strings)
  putStr " | "
  print (solve2 5 60 strings)
  putStrLn ""
