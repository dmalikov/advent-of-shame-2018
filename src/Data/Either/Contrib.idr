module Data.Either.Contrib

%default total
%access public export


||| Convert an Either to a Maybe from Right injection
eitherToMaybe : Either e a -> Maybe a
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x
