module Data.CSV.Validator
  ( ValidCSV
  , ValidateError(..)
  , CSVInfo
  , getCSVInfo
  , validate
  ) where

import Prelude
import Data.CSV (CSV(..))
import Data.Either (Either(..))
import Data.Foldable (any, length)
import Data.List.NonEmpty (NonEmptyList, cons, uncons)

data ValidCSV
  = ValidCSV CSV

data ValidateError
  = ValidateError CSVInfo

type CSVInfo
  = { lines :: Int
    , cols :: NonEmptyList Int
    }

getCSVInfo :: CSV -> CSVInfo
getCSVInfo (WithHeader header body) =
  { lines: 1 + (length body)
  , cols: length header `cons` map length body
  }

getCSVInfo (WithoutHeader body) =
  { lines: length body
  , cols: map length body
  }

validate :: CSV -> Either ValidateError ValidCSV
validate csv = case uncons info.cols of
  { head, tail: tail } ->
    if any (eq head) tail then
      pure $ ValidCSV csv
    else
      Left $ ValidateError info
  where
  info = getCSVInfo csv
