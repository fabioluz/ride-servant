module Ride.Shared.Validators.Text
( notEmpty
, pattern
, minLength
, validateText
, TextError (..)
) where

data TextError
  = NotEmptyError
  | MaxLenthError Int
  | MinLengthError Int
  | PatternError Text
  deriving Show

validateText :: [(Text -> Either TextError Text)] -> Text -> Either TextError Text
validateText xs a = foldl' (>>=) (Right a) xs

validate :: e -> (a -> Bool) -> a -> Either e a
validate e f a = if f a 
                 then Right a
                 else Left e

notEmpty :: Text -> Either TextError Text
notEmpty = validate NotEmptyError (not . null)

minLength :: Int -> Text -> Either TextError Text
minLength n = validate (MinLengthError n) ((<=) n . length)

-- | TODO: replace for Regex
pattern :: Text -> Text -> Either TextError Text
pattern p = validate (PatternError p) (isInfixOf p)