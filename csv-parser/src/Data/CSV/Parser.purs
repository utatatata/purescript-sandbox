-- | Reference: RFC 4180 (https://tools.ietf.org/html/rfc4180#section-2)
module Data.CSV.Parser where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array as A
import Data.CSV (CSV(..), CSVBody, CSVField(..), CSVHeader, CSVName, CSVRecord)
import Data.Char (toCharCode)
import Data.Either (Either)
import Data.String.CodeUnits (fromCharArray)
import Text.Parsing.Parser (ParseError, Parser, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, sepEndBy1)
import Text.Parsing.Parser.String (char, eof, satisfy)

parse :: String -> Either ParseError CSV
parse file = runParser file csv

csv :: Parser String CSV
csv = file <* eof
  where
  file :: Parser String CSV
  file = withHeader <|> withoutHeader

  withHeader :: Parser String CSV
  withHeader = WithHeader <$> header <* crlf <*> body

  withoutHeader :: Parser String CSV
  withoutHeader = WithoutHeader <$> body

  header :: Parser String CSVHeader
  header = name `sepBy1` comma

  body :: Parser String CSVBody
  body = record `sepEndBy1` crlf

  record :: Parser String CSVRecord
  record = field `sepBy1` comma

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
  crlf = (\c1 c2 -> fromCharArray [c1, c2]) <$> cr <*> lf

  textdata :: Parser String Char
  textdata = satisfy isTextdata

  -- | %x20-21 / %x23-2B / %x2D-7E
  isTextdata :: Char -> Boolean
  isTextdata c = 32 <= uc || uc <= 33 || 35 <= uc || uc <= 43 || 45 <= uc || uc <= 126
    where
    uc :: Int
    uc = toCharCode c
