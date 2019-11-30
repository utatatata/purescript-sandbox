module Data.CSV where

import Data.List.NonEmpty (NonEmptyList)

data CSV
  = WithHeader CSVHeader CSVBody
  | WithoutHeader CSVBody

type CSVHeader = NonEmptyList CSVName

type CSVBody = NonEmptyList CSVRecord

type CSVRecord = NonEmptyList CSVField

type CSVName = CSVField

data CSVField
  = Escaped String
  | NonEscaped String