module Test.Data.CSV.Validator where

import Prelude
import Data.CSV (CSV(..), CSVField(..))
import Data.CSV.Validator (validate)
import Data.Either (Either(..), isLeft)
import Data.List (fromFoldable)
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.Newtype (unwrap)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)

testValidate :: Spec Unit
testValidate =
  describe "Data.CSV.Validator" do
    describe "validate" do
      it "is valid csv" do
        case validate csv1 of
          Left _ -> fail "validate is failed"
          Right validCsv -> unwrap validCsv `shouldEqual` csv1
      it "is invalid csv" do
        validate csv2 `shouldSatisfy` isLeft
  where
  from :: forall a. a -> Array a -> NonEmptyList a
  from x xs = cons' x $ fromFoldable xs

  csv1 :: CSV
  csv1 =
    WithHeader
      (map NonEscaped $ from "h1" [ "h2", "h3" ])
      $ map (map NonEscaped)
      $ from
          (from "aaa" [ "bbb", "ccc" ])
          [ from "ddd" [ "eee", "fff" ]
          ]

  csv2 :: CSV
  csv2 =
    WithoutHeader
      $ map (map NonEscaped)
      $ from
          (from "aaa" [ "bbb", "ccc" ])
          [ from "ddd" []
          ]
