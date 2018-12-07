module Data.List.Contrib

%default total
%access public export


group' : (Eq a) => (current : a) -> (cnt : Nat) -> List a -> List (a, Nat)
group' current cnt (x :: xs) =
  if current == x
    then group' current (cnt + 1) xs
    else (current, cnt) :: group' x 1 xs
group' current cnt [] = [(current, cnt)]

-- | Group identical elements.
-- ["a", "b", "a", "b", "b", "a"] -> [("a", 1), ("b", 1), ("a", 1), ("b", 2), ("a", 1)]
group : (Eq a) => List a -> List (a, Nat)
group (x :: xs) = group' x 1 xs
group [] = []
