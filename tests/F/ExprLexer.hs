-- | Provides tokens and a lexer for a simple arithmetic expression language.
module F.ExprLexer (
    -- * The token datatype
    ExprToken(..), isSpace, isNum,
    
    -- * Parsing tokens
    CharParser, pTokens
  ) where

import Annotations.BoundsParser

import qualified Text.Parsec as P

import Data.Maybe (fromJust)
import Control.Applicative


-- | Tokens in the language.
data ExprToken
  =  TNum    Int
  |  TPlus
  |  TMinus
  |  TStar
  |  TSlash
  |  TOpen
  |  TClose
  |  TSpace  String
  deriving (Eq, Show)

instance Symbol ExprToken where
  unparse t = case t of
    TNum n -> show n
    TSpace s -> s
    _ -> fromJust (lookup t statics)

-- | True iff it is a 'TNum'.
isNum :: ExprToken -> Bool
isNum (TNum _)  = True
isNum _         = False

-- | True iff it is a 'TSpace'.
isSpace :: ExprToken -> Bool
isSpace (TSpace _)  = True
isSpace _           = False


-- | A parser without user state that works on strings.
type CharParser = P.Parsec String ()

-- | A parses that recognises a stream of 'ExprToken's.
pTokens :: CharParser [ExprToken]
pTokens = many (P.choice [pStaticToken, pInt, pSpace])

statics :: [(ExprToken, String)]
statics =
  [ (TPlus, "+")
  , (TMinus, "-")
  , (TStar, "*")
  , (TSlash, "/")
  , (TOpen, "(")
  , (TClose, ")")
  ]

pStaticToken :: CharParser ExprToken
pStaticToken = P.choice $ map (\(tok, syn) -> tok <$ P.string syn) statics

pSpace :: CharParser ExprToken
pSpace = TSpace <$> some (P.oneOf " \n\r\t\f")

pInt :: CharParser ExprToken
pInt = TNum 0 <$ P.char '0'
   <|> (\n -> TNum . read . (n:)) <$> P.oneOf ['1'..'9'] <*> many (P.oneOf ['0'..'9'])
