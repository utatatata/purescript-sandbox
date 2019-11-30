-- | Reference: RFC 4180 (https://tools.ietf.org/html/rfc4180)
module Data.CSV.Parser where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Control.Monad.State (gets)
import Data.Array as A
import Data.CSV (CSV(..), CSVBody, CSVField(..), CSVHeader, CSVName, CSVRecord)
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.List (List(..), many)
import Data.List.NonEmpty (cons')
import Data.String as S
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParseError, ParseState(..), Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (between, sepEndBy)
import Text.Parsing.Parser.String (char, eof, satisfy)

type Param
  = { header :: ParamHeader
    }

data ParamHeader
  = Presence
  | Absence

parse :: Param -> String -> Either ParseError CSV
parse param str = runParser str $ csv param

csv :: Param -> Parser String CSV
csv param = file <* eof
  where
  file :: Parser String CSV
  file = case param.header of
    Presence -> withHeader
    Absence -> withoutHeader

  withHeader :: Parser String CSV
  withHeader = WithHeader <$> header <* crlf <*> body

  withoutHeader :: Parser String CSV
  withoutHeader = WithoutHeader <$> body

  header :: Parser String CSVHeader
  header = cons' <$> name <*> many (comma *> name)

  -- | The last record int the file may or may have an ending line break.
  -- | So 'sepEndBy' is used.
  body :: Parser String CSVBody
  body =
    record `sepEndBy` crlf
      >>= case _ of
          Nil -> fail "requires one or more records"
          Cons x xs -> pure $ cons' x xs

  record :: Parser String CSVRecord
  -- record = cons' <$> field <*> many (comma *> field)
  record = do
    input <- gets \(ParseState input _ _) -> input
    if S.null input then
      fail "Records that have only one non-escaped field and end with EOF are not allowed."
    else
      cons' <$> field <*> many (comma *> field)

  name :: Parser String CSVName
  name = field

  field :: Parser String CSVField
  field = escaped <|> nonEscaped

  escaped :: Parser String CSVField
  escaped = Escaped <<< fromCharArray <$> (between dQuote dQuote $ A.many $ textdata <|> comma <|> cr <|> lf <|> dQuote)

  nonEscaped :: Parser String CSVField
  nonEscaped = NonEscaped <<< fromCharArray <$> A.many textdata

  comma :: Parser String Char
  comma = char ','

  cr :: Parser String Char
  cr = char '\r'

  dQuote :: Parser String Char
  dQuote = char '"'

  lf :: Parser String Char
  lf = char '\n'

  crlf :: Parser String String
  crlf = (\c1 c2 -> fromCharArray [ c1, c2 ]) <$> cr <*> lf

  textdata :: Parser String Char
  textdata = satisfy isTextdata

  -- | %x20-21 / %x23-2B / %x2D-7E
  isTextdata :: Char -> Boolean
  isTextdata c = (32 <= uc && uc <= 33) || (35 <= uc && uc <= 43) || (45 <= uc && uc <= 126)
    where
    uc :: Int
    uc = toCharCode c
