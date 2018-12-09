module Day09

import Data.Vect as V
import Data.SortedMap as SM

import Core

%default total
%access public export


Marble : Type
Marble = Integer

Score : Type
Score = Integer

record Circle where
  constructor MkCircle
  current : Marble
  rest : List Marble

init : Circle
init = MkCircle 0 []

add_non23ish : Marble -> Circle -> Circle
add_non23ish marble circle = let (head, tail) = splitAt 1 (rest circle) in
  record
    { current = marble
    , rest = tail ++ [current circle] ++ head
    } circle

-- Something differently happens.
-- The marble 7 marbles counter-clockwise from the current marble is
--   removed from the circle and also added to the current player's score.
--   The marble located immediately clockwise of the marble that was
--   removed becomes the new current marble.
-- [ 11, 1, 12, 6, ..., 4, 18, 9, 19, 2, 20, 10, 21, 5 ]
--  \----     head      ----/  ^   ^  \---- tail ----/
--                             |   |---- current'
--                             |-- deleted
-- -> (deleted, current', tail, head)
-- == (9, 19, [2, 20, 10, 21, 5], [11, 1, 12, 6, ..., 4, 18])
--
-- TODO total
partial move7 : List Marble -> (Marble, Marble, List Marble, List Marble)
move7 xs =
  let (deleted :: current' :: tail) = reverse (Prelude.List.take 7 (reverse xs))
      head = Prelude.List.take (fromIntegerNat (toIntegerNat (Prelude.List.length xs) - 7)) xs
   in (deleted, current', tail, head)

partial add_23ish : Marble -> Circle -> (Circle, Score)
add_23ish marble (MkCircle current rest) =
  let (deleted, current', t, h) = move7 rest
      rest' = t ++ [current] ++ h
      score = deleted + marble
   in (MkCircle current' rest', score)

partial add : Marble -> Circle -> (Circle, Score)
add marble circle =
  if marble `mod` 23 == 0
    then add_23ish marble circle
    else (add_non23ish marble circle, 0)

-- 1 to N
Player : Type
Player = Integer

record Game where
  constructor MkGame
  move : Player
  players : Integer
  circle : Circle
  scores : SortedMap Player Score

next : Integer -> Player -> Player
next players current =
  if current == players
    then 1
    else current + 1

insert_score : Player -> Score -> SortedMap Player Score -> SortedMap Player Score
insert_score _ 0 scores = scores
insert_score player score scores =
  case lookup player scores of
    Just score' => insert player (score' + score) scores
    Nothing     => insert player  score           scores

partial step : Marble -> Game -> Game
step marble game =
  let (circle', score) = add marble (circle game)
   in record
        { move $= next (players game)
        , scores $= insert_score (move game) score
        , circle = circle'
        } game

high_score : SortedMap Player Score -> Score
high_score = fromMaybe 0 . listToMaybe . reverse . sort . values

partial run_game : Marble -> Integer -> Game -> Game
run_game marble marbles game =
  if marble > marbles
    then game
    else run_game (marble + 1) marbles (step marble game)

partial solve : Integer -> Integer -> Score
solve players marbles =
  high_score $
  scores $
  run_game 1 marbles $
  MkGame 1 players init empty

partial assertions_aux : IO ()
assertions_aux = do
  let (MkGame move players (MkCircle current rest) scores) =
         run_game 1 25 $ MkGame 1 9 init empty
  assert 8 move
  assert 9 players
  assert 25 current
  let exp_rest : List Marble = [ 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15, 0, 16, 8, 17, 4, 18, 19, 2, 24, 20 ]
  assert exp_rest rest
  assert [(5, 32)] (toList scores)

partial day09 : IO ()
day09 = do
  -- assertions_aux
  putStr "Day09: "
  assert 32     (solve 9  25  )
  assert 8317   (solve 10 1618)
  assert 146373 (solve 13 7999)
  assert 2764   (solve 17 1104)
  assert 54718  (solve 21 6111)
  assert 37305  (solve 30 5807)
  print (solve 486 70833)
  putStr " | "
  print (solve 486 (70833 * 100))
  putStrLn ""
