module Day03

import Data.List
import Data.SortedMap
import Effects
import Effect.Exception
import Effect.Monad
import Lightyear
import Lightyear.Char
import Lightyear.Strings

import Core

%default total
%access public export

ClaimId : Type
ClaimId = String

Pad : Type
Pad = Integer

Size : Type
Size = Integer

data Overlap = Single ClaimId | Many

Show Overlap where
  show (Single claimId) = "Single " ++ show claimId
  show Many             = "Many"

toClaimId : Overlap -> Maybe ClaimId
toClaimId (Single c) = Just c
toClaimId            _ = Nothing

overlaps : Overlap -> Overlap -> Overlap
overlaps _ _ = Many

Eq Overlap where
  (Single s) == (Single t) = s == t
  Many == Many             = True
  _ == _                   = False

Fabric : Type
Fabric = SortedMap (Integer, Integer) Overlap

record Claim where
  constructor MkClaim
  claimId : ClaimId
  padX, padY : Pad
  sizeX, sizeY : Size

-- "#1 @ 1,3: 4x4" -> Claim { claimId = "1", padX = 1, padY = 3, sizeX = 4, sizeY = 4 }
partial claim : Parser Claim
claim = MkClaim
  <$> (string "#" *> spaces *> (pack <$> some (satisfy (/= ' '))))
  <*> (spaces *> string "@" *> spaces *> (fromDigits <$> some digit))
  <*> (spaces *> string "," *> spaces *> (fromDigits <$> some digit))
  <*> (spaces *> string ":" *> spaces *> (fromDigits <$> some digit))
  <*> (spaces *> string "x" *>           (fromDigits <$> some digit))

fabric : Claim -> Fabric
fabric (MkClaim claimId padX padY sizeX sizeY) = fromList (zip fill (replicate (length fill) (Single claimId)))
 where
  fill : List (Integer, Integer)
  fill = do
    x <- [(padX + 1) .. (padX + sizeX)]
    y <- [(padY + 1) .. (padY + sizeY)]
    pure (x, y)

-- TODO: this seems to be a bottleneck in the performance, figure out why
overlap : List Fabric -> Fabric
overlap = foldl (mergeWith overlaps) empty

partial solve1 : List String -> Nat
solve1 = List.length . List.filter (== Many) . values . overlap . map fabric . catMaybes . map (parse' claim)

partial solve2 : List String -> List ClaimId
solve2 strings =
  let claims : List Claim = catMaybes . map (parse' claim) $ strings
      claimSizes : List (ClaimId, Integer) = map (\(MkClaim claimId _ _ sizeX sizeY) => (claimId, sizeX * sizeY)) claims
      nonOverlapped : List (ClaimId, Integer) = map (\(a, b) => (a, toIntegerNat b)) .  group . sort . catMaybes . map toClaimId . values . overlap . map fabric $ claims
   in map fst $ intersect claimSizes nonOverlapped

partial day03 : IO ()
day03 = do
  putStr "Day 03: "
  assert 4 (solve1
    [ "#1 @ 1,3: 4x4"
    , "#2 @ 3,1: 4x4"
    , "#3 @ 5,5: 2x2"
    ])
  assert ["3"] (solve2
    [ "#1 @ 1,3: 4x4"
    , "#2 @ 3,1: 4x4"
    , "#3 @ 5,5: 2x2"
    ])
  strings <- readFile' "input/day03.txt"
  print (solve1 strings)
  putStr " | "
  print (solve2 strings)
  putStrLn ""
