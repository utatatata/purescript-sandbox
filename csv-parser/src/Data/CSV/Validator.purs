module Data.CSV.Validator where

import Prelude
import Data.CSV (CSV(..))
import Data.Either (Either(..))
import Data.Foldable (any, length)
import Data.List.NonEmpty (NonEmptyList, cons, uncons)
import Data.Newtype (class Newtype)

newtype ValidCSV
  = ValidCSV CSV

derive instance newtypeValidCSV :: Newtype ValidCSV _

instance showValidCSV :: Show ValidCSV where
  show (ValidCSV csv) = "(ValidCSV " <> show csv <> ")"

data ValidateError
  = ValidateError CSVInfo

instance showValidateError :: Show ValidateError where
  show (ValidateError info) = "(ValidateError " <> show info <> ")"

newtype CSVInfo
  = CSVInfo
  { lines :: Int
  , cols :: NonEmptyList Int
  }

instance showCSVInfo :: Show CSVInfo where
  show (CSVInfo { lines, cols }) = "(CSVInfo { lines: " <> show lines <> ", cols: " <> show cols <> " })"

getCSVInfo :: CSV -> CSVInfo
getCSVInfo (WithHeader header body) =
  CSVInfo
    { lines: 1 + (length body)
    , cols: length header `cons` map length body
    }

getCSVInfo (WithoutHeader body) =
  CSVInfo
    { lines: length body
    , cols: map length body
    }

validate :: CSV -> Either ValidateError ValidCSV
validate csv = case uncons info.cols of
  { head, tail: tail } ->
    if any (eq head) tail then
      pure $ ValidCSV csv
    else
      Left $ ValidateError $ CSVInfo info
  where
  CSVInfo info = getCSVInfo csv
