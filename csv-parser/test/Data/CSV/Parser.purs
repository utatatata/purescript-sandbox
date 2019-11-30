module Test.Data.CSV.Parser where

import Prelude
import Data.CSV (CSV(..), CSVField(..))
import Data.CSV.Parser (parse, ParamHeader(..))
import Data.Either (Either(..))
import Data.List (fromFoldable)
import Data.List.NonEmpty (NonEmptyList, cons')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

testParse :: Spec Unit
testParse =
  describe "Data.CSV.Parser" do
    describe "parse" do
      it "has no header" do
        parse { header: Absence } "aaa,bbb,ccc\r\nddd,eee,fff\r\n"
          `shouldEqual`
            Right
              ( WithoutHeader $ map (map NonEscaped)
                  $ from
                      (from "aaa" [ "bbb", "ccc" ])
                      [ from "ddd" [ "eee", "fff" ]
                      ]
              )
      it "has no header and no CRLF in the last line" do
        parse { header: Absence } "aaa,bbb,ccc\r\nddd,eee,fff"
          `shouldEqual`
            Right
              ( WithoutHeader $ map (map NonEscaped)
                  $ from
                      (from "aaa" [ "bbb", "ccc" ])
                      [ from "ddd" [ "eee", "fff" ]
                      ]
              )
      it "has no header, one colum and non-escaped field" do
        parse { header: Absence } "\r\n\r\n"
          `shouldEqual`
            Right
              ( WithoutHeader $ map (map NonEscaped)
                  $ from
                      (from "" [])
                      [ from "" []
                      ]
              )
      it "has a header, one colum and non-escaped field" do
        parse { header: Presence } "aaa\r\n\r\n"
          `shouldEqual`
            Right
              ( WithHeader
                  (map NonEscaped $ from "aaa" [])
                  $ map (map NonEscaped)
                  $ from
                      (from "" [])
                      []
              )
  where
  from :: forall a. a -> Array a -> NonEmptyList a
  from x xs = cons' x $ fromFoldable xs
