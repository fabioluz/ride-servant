{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE InstanceSigs #-}

module Ride.Shared.Utils where

import Control.Monad.Except (MonadError, liftEither)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Language.Haskell.TH.Syntax (Name, Dec, Q)
import Servant (ServerError, throwError, err422, errBody)
import Ride.App (AppError (..))

import qualified Data.ByteString.Lazy.Char8 as BS

lowerCaseFirst :: String -> String
lowerCaseFirst (x:xs) = toLower [x] <> xs
lowerCaseFirst ""     = ""

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy term = splitBy' []
  where
    splitBy' acc []  = filter (not . null) acc
    splitBy' acc src = splitBy' newAcc newSrc
      where
        res = span (\x -> x /= term) src
        newAcc = acc <> [fst res]
        newSrc = drop 1 $ snd res

commonDeriveJSON :: Name -> Q [Dec]
commonDeriveJSON name = deriveJSON options name
  where
    options = defaultOptions { fieldLabelModifier = modifier }
    modifier = lowerCaseFirst . drop (length structName)
    structName = fromMaybe "" . fmap last . fromNullable . splitBy '.' $ show name

commonDeriveManyJSON :: [Name] -> Q [Dec]
commonDeriveManyJSON = fmap concat . traverse commonDeriveJSON

