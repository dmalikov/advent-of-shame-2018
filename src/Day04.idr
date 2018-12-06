module Day04

import Data.SortedMap
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Core

%default total
%access public export

Guard : Type
Guard = Integer

Day : Type
Day = String

data Hour = H00 | H23

Show Hour where
  show H00 = "H00"
  show H23 = "H23"

Minute : Type
Minute = Integer

record Time where
  constructor MkTime
  day : Day
  hour : Hour
  min : Minute

Show Time where
  show (MkTime day hour min) = day ++ " " ++ show hour ++ "h " ++ show min ++ "min"

data Event
  = GuardShift Time Guard
  | FallAsleep Time
  | WakesUp Time

Show Event where
  show (GuardShift time guard) = "GuardShift at " ++ show time ++ " by " ++ show guard
  show (FallAsleep time      ) = "FallAsleep at " ++ show time
  show (WakesUp    time      ) = "WakesUp at "    ++ show time

NapMap : Type
NapMap = SortedMap Minute Nat

Naps : Type
Naps = SortedMap Guard NapMap

partial groupGuards : List (Guard, List (Minute, Minute)) -> List (Guard, List (Minute, Minute))
groupGuards [] = []
groupGuards (x :: []) = [x]
groupGuards ((g, xminutes) :: (g', yminutes) :: xs) =
  if g == g'
    then groupGuards ((g, xminutes ++ yminutes) :: xs)
    else (g, xminutes) :: groupGuards ((g', yminutes) :: xs)

freq_minutes : List (Minute, Minute) -> List (Minute, Nat)
freq_minutes = group . sort . concatMap (\(f, t) => [f .. (t - 1)])

partial mkNaps : List (Guard, List (Minute, Minute)) -> Naps
mkNaps = fromList . map (\(x,y) => (x, fromList . freq_minutes $ y)) . groupGuards . sort

maxBy : Ord b => (a -> b) -> List a -> Maybe a
maxBy f = map fst . listToMaybe . reverse . sortBy (compare `on` snd) . map (\x => (x, f x))

most_sleepy_overall : Naps -> Maybe (Guard, NapMap)
most_sleepy_overall = maxBy (sum . values . snd) . toList

most_sleepy_same_time : Naps -> Maybe (Guard, NapMap)
most_sleepy_same_time = maxBy (listToMaybe . reverse . sort . values . snd) . toList

guard_x_minute : (Guard, NapMap) -> Integer
guard_x_minute (g, minutes) = g * (fromMaybe 0 . map Basics.fst . maxBy snd . toList $ minutes)

partial guardP : Parser Guard
guardP = fromDigits <$> some digit

-- "00" -> H00
-- "23" -> H23
hourP : Parser Hour
hourP = do
  d1 <- digit
  d2 <- digit
  case (finToInteger d1, finToInteger d2) of
    (0, 0) => pure H00
    (2, 3) => pure H23
    _      => empty

minuteP : Parser Minute
minuteP = do
  d1 <- finToInteger <$> digit
  d2 <- finToInteger <$> digit
  pure (10 * d1 + d2)

-- "1518-11-01 00:00" -> Time "1518-11-01" H00 00
partial timeP : Parser Time
timeP = MkTime
  <$> (pack <$> some (satisfy (/= ' ')) <* spaces)
  <*> (hourP <* string ":")
  <*> minuteP

partial eventP : Parser Event
eventP = do
  time <- string "[" *> timeP <* string "]" <* spaces
  e <- pack <$> some (satisfy (/= '#'))
  case e of
    "wakes up"     => pure (WakesUp time)
    "falls asleep" => pure (FallAsleep time)
    _ => do
      g <- string "#" *> guardP <* string " begins shift"
      pure (GuardShift time g)

partial sort_parse : List String -> List Event
sort_parse = catMaybes . map (parse' eventP) . sort

group_events' : Guard -> List (Minute, Minute) -> List Event -> List (Guard, List (Minute, Minute))
group_events' g agg [] = [(g, agg)]
group_events' g agg ((GuardShift _ guard) :: xs) = [(g, agg)] ++ (group_events' guard [] xs)
group_events' g agg ((FallAsleep x) :: (WakesUp y) :: xs) = group_events' g ((min x, min y) :: agg) xs
group_events' g agg _ = []

group_events : List Event -> List (Guard, List (Minute, Minute))
group_events [] = []
group_events ((GuardShift _ guard) :: xs) = group_events' guard [] xs
group_events ((FallAsleep _) :: xs) = group_events xs
group_events ((WakesUp _) :: xs) = group_events xs

partial solve1 : List String -> Integer
solve1 = maybe 0 guard_x_minute . most_sleepy_overall . mkNaps . group_events . sort_parse

partial solve2 : List String -> Integer
solve2 = maybe 0 guard_x_minute . most_sleepy_same_time . mkNaps . group_events . sort_parse

test_input : List String
test_input =
  [ "[1518-11-01 00:00] Guard #10 begins shift"
  , "[1518-11-01 00:05] falls asleep"
  , "[1518-11-01 00:25] wakes up"
  , "[1518-11-01 00:30] falls asleep"
  , "[1518-11-01 00:55] wakes up"
  , "[1518-11-01 23:58] Guard #99 begins shift"
  , "[1518-11-02 00:40] falls asleep"
  , "[1518-11-02 00:50] wakes up"
  , "[1518-11-03 00:05] Guard #10 begins shift"
  , "[1518-11-03 00:24] falls asleep"
  , "[1518-11-03 00:29] wakes up"
  , "[1518-11-04 00:02] Guard #99 begins shift"
  , "[1518-11-04 00:36] falls asleep"
  , "[1518-11-04 00:46] wakes up"
  , "[1518-11-05 00:03] Guard #99 begins shift"
  , "[1518-11-05 00:45] falls asleep"
  , "[1518-11-05 00:55] wakes up"
  ]

partial day04 : IO ()
day04 = do
  putStr "Day 04: "
  assert 240 (solve1 test_input)
  assert 4455 (solve2 test_input)
  strings <- readFile' "input/day04.txt"
  print (solve1 strings)
  putStr " | "
  print (solve2 strings)
  putStrLn ""
