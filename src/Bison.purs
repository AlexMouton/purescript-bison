module Bison where

import Prelude

import Control.Alt ((<|>))
import Control.Apply (lift2)

import Data.NonEmpty (NonEmpty(..))
import Data.List (List(..), concat)
import Data.List.Types (NonEmptyList(..))
import Data.Foldable (foldl)
import Data.String.CodeUnits (singleton)

import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.CodePoints as Cp
import Text.Parsing.StringParser.Combinators as Co

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

parseGrammar :: Parser (List Rule)
parseGrammar =
  Cp.string "%%"
  *> Cp.skipSpaces
  *> parseRules
  <* Cp.skipSpaces

parseRules :: Parser (List Rule)
parseRules = Cp.skipSpaces *> (Co.sepEndBy parseRule Cp.whiteSpace)

type Rule = { name :: String, terms :: NonEmptyList Term }

ruleCons :: ∀ a. a -> List a -> NonEmptyList a
ruleCons a b = NonEmptyList $ NonEmpty a b

parsePipe :: Parser Unit
parsePipe = void $ Cp.skipSpaces *> Cp.char '|' *> Cp.skipSpaces

parseTerms = (Co.sepBy1 parseTerm parsePipe)
  <* Cp.skipSpaces
  <* Cp.char ';'

parseRule :: Parser Rule
parseRule = { name : _,  terms: _ }
  <$> (Cp.skipSpaces *> parseName <* Cp.skipSpaces)
  <*>
    ( Cp.char ':'
      *> Cp.skipSpaces
      *> parseTerms
    )

parseName :: Parser String
parseName = Cp.regex "[A-Z][A-Za-z0-9]+"

type Term = (NonEmptyList Point)
parseTerm :: Parser Term
parseTerm = Co.sepEndBy1 parsePoint Cp.whiteSpace

data Point = Tk Token | Rx Regx | Rf Ref | Lt Literal | Ep Epsilon
derive instance genericPoint :: Generic Point _
instance showPoint :: Show Point where
  show = genericShow

parsePoint :: Parser Point
parsePoint =
  (Tk <$> parseToken)
  <|> (Rf <$> parseRef)
  <|> (Rx <$> parseRegx)
  <|> (Lt <$> parseLiteral)
  <|> (Ep <$> parseEpsilon)

pointParser :: Point -> Parser String
pointParser (Tk a) = tokenParser a
pointParser (Rf a) = refParser a
pointParser (Rx a) = regxParser a
pointParser (Lt a) = literalParser a
pointParser (Ep a) = epsilonParser a

data Token = Token String
derive instance genericToken :: Generic Token _
instance showToken :: Show Token where
  show = genericShow

parseToken :: Parser Token
parseToken = Token <$> Cp.regex "[a-z]+"

tokenParser :: Token -> Parser String
tokenParser (Token s) = Cp.string s

data Ref = Ref String
derive instance genericRef :: Generic Ref _
instance showRef :: Show Ref where
  show = genericShow

parseRef :: Parser Ref
parseRef = Ref <$> parseName

refParser :: Ref -> Parser String
refParser (Ref s) = Cp.string s


-- A named token type is written with an identifier, like an identifier in C. By convention, it should be all upper case. Each such name must be defined with a Bison declaration such as %token. See section Token Type Names.
data Regx = Regx String
derive instance genericRegx :: Generic Regx _
instance showRegx :: Show Regx where
  show = genericShow

parseRegx :: Parser Regx
parseRegx = Regx <$> Cp.regex "[A-Z]+"

regxParser :: Regx -> Parser String
regxParser (Regx s) = Cp.string s

-- A character token type (or literal character token) is written in the grammar using the same syntax used in C for character constants; for example, '+' is a character token type. A character token type doesn't need to be declared unless you need to specify its semantic value data type (see section Data Types of Semantic Values), associativity, or precedence (see section Operator Precedence). By convention, a character token type is used only to represent a token that consists of that particular character. Thus, the token type '+' is used to represent the character `+' as a token. Nothing enforces this convention, but if you depart from it, your program will confuse other readers. All the usual escape sequences used in character literals in C can be used in Bison as well, but you must not use the null character as a character literal because its ASCII code, zero, is the code yylex returns for end-of-input (see section Calling Convention for yylex).
-- A literal string token is written like a C string constant; for example, "<=" is a literal string token. A literal string token doesn't need to be declared unless you need to specify its semantic value data type (see section Data Types of Semantic Values), associativity, precedence (see section Operator Precedence). You can associate the literal string token with a symbolic name as an alias, using the %token declaration (see section Token Type Names). If you don't do that, the lexical analyzer has to retrieve the token number for the literal string token from the yytname table (see section Calling Convention for yylex). WARNING: literal string tokens do not work in Yacc. By convention, a literal string token is used only to represent a token that consists of that particular string. Thus, you should use the token type "<=" to represent the string `<=' as a token. Bison does not enforces this convention, but if you depart from it, people who read your program will be confused. All the escape sequences used in string literals in C can be used in Bison as well. A literal string token must contain two or more characters; for a token containing just one character, use a character token (see above).
data Literal = LiteralChar Char | LiteralString String
derive instance genericLiteral :: Generic Literal _
instance showLiteral :: Show Literal where
  show = genericShow

parseLiteral :: Parser Literal
parseLiteral = parseLiteralChar <|> parseLiteralString

parseLiteralChar :: Parser Literal
parseLiteralChar = LiteralChar <$> (Cp.char '\'' *> Cp.anyChar <* Cp.char '\'')

literalParser :: Literal -> Parser String
literalParser (LiteralChar c) = singleton <$> Cp.char c
literalParser (LiteralString s) = Cp.string s

parseLiteralString :: Parser Literal
parseLiteralString = LiteralString <$> ((Cp.char '"') *> (Cp.regex "[^\"]+") <* (Cp.char '"'))

data Epsilon = Epsilon
derive instance genericEpsilon :: Generic Epsilon _
instance showEpsilon :: Show Epsilon where
  show = genericShow

parseEpsilon :: Parser Epsilon
parseEpsilon = Epsilon <$ (Cp.skipSpaces *> Cp.string "ε" <* Cp.skipSpaces)

epsilonParser :: Epsilon -> Parser String
epsilonParser _ = pure ""
