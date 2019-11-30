module Data.CSV.Validator
  ( ValidCSV
  , ValidateError(..)
  , CSVInfo
  , getCSVInfo
  , validate
  )
  where

import Prelude

import Data.CSV (CSV(..))
import Data.Either (Either(..))
import Data.Foldable (any, length)
import Data.List (List(..), (:))

data ValidCSV = ValidCSV CSV

data ValidateError = ValidateError CSVInfo

type CSVInfo =
  { lines :: Int
  , cols :: List Int
  }

getCSVInfo :: CSV -> CSVInfo
getCSVInfo (WithHeader header body) =
  { lines: 1 + (length body)
  , cols: length header : map length body
  }
getCSVInfo (WithoutHeader body) =
  { lines: length body
  , cols: map length body
  }

validate :: CSV -> Either ValidateError ValidCSV
validate csv = case info.cols of
    Nil -> Left $ ValidateError info
    xs@(Cons x _) ->
      if any (eq x) xs then
        pure $ ValidCSV csv
      else
        Left $ ValidateError info
  where
  info = getCSVInfo csv
