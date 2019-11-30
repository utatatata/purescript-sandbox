module Data.CSV where

import Prelude
import Data.List.NonEmpty (NonEmptyList)

data CSV
  = WithHeader CSVHeader CSVBody
  | WithoutHeader CSVBody

derive instance eqCSV :: Eq CSV

instance showCSV :: Show CSV where
  show (WithHeader header body) = "(WithHeader " <> show header <> " " <> show body <> ")"
  show (WithoutHeader body) = "(WithoutHeader " <> show body <> ")"

type CSVHeader
  = NonEmptyList CSVName

type CSVBody
  = NonEmptyList CSVRecord

type CSVRecord
  = NonEmptyList CSVField

type CSVName
  = CSVField

data CSVField
  = Escaped String
  | NonEscaped String

derive instance eqCSVField :: Eq CSVField

instance showCSVField :: Show CSVField where
  show (Escaped s) = "(Escaped " <> show s <> ")"
  show (NonEscaped s) = "(NonEscaped " <> s <> ")"
