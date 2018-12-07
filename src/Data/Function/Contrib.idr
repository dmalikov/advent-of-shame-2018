module Data.Function.Contrib

%default total
%access public export


-- | 'Data.Function.on'
on : (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f x y = g (f x) (f y)
