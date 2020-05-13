module Ride.Shared.Validators.Text
( notEmpty
, matches
, minLength
, validateText
, TextError
) where

data TextError
  = NotEmptyError
  | MaxLenthError Int
  | MinLengthError Int
  | MatchesError Text
  deriving Show

validateText :: [Text -> Either TextError Text] -> Text -> Either TextError Text
validateText xs a = foldl' (>>=) (Right a) xs

validate :: e -> (a -> Bool) -> a -> Either e a
validate e f a
  | f a       = Right a
  | otherwise = Left e

notEmpty :: Text -> Either TextError Text
notEmpty = validate NotEmptyError (not . null)

minLength :: Int -> Text -> Either TextError Text
minLength n = validate (MinLengthError n) ((<=) n . length)

-- | TODO: replace for Regex
matches :: Text -> Text -> Either TextError Text
matches p = validate (MatchesError p) (isInfixOf p)
