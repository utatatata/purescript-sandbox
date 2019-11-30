module Data.CSV where

import Data.List (List)

data CSV
  = WithHeader CSVHeader CSVBody
  | WithoutHeader CSVBody

type CSVHeader = List CSVName

type CSVBody = List CSVRecord

type CSVRecord = List CSVField

type CSVName = CSVField

data CSVField
  = Escaped String
  | NonEscaped String