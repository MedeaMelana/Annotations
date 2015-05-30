module MultiRec.ExprLexer (ExprToken(..), isSpace, isIntLit, pTokens) where

import Annotations.BoundsParser

import qualified Text.Parsec as P

import Data.Maybe (fromJust)
import Control.Applicative


data ExprToken
  = TIntLit Int
  | TPlus
  | TStar
  | TLParen
  | TRParen
  | TDoubleColon
  | TInt
  | TComma
  | TSpace String
  deriving (Eq, Show)

instance Symbol ExprToken where
  unparse t = case t of
    TIntLit n -> show n
    TSpace s -> s
    -- TVar s -> s
    _ -> fromJust (lookup t statics)

-- | True iff it is a 'TIntLit'.
isIntLit :: ExprToken -> Bool
isIntLit (TIntLit _)  = True
isIntLit _            = False

-- -- | True iff it is a 'TVar'.
-- isVar :: ExprToken -> Bool
-- isVar (TVar _)  = True
-- isVar _         = False

-- | True iff it is a 'TSpace'.
isSpace :: ExprToken -> Bool
isSpace (TSpace _)  = True
isSpace _           = False

statics :: [(ExprToken, String)]
statics =
  [ (TPlus, "+")
  , (TStar, "*")
  , (TLParen, "(")
  , (TRParen, ")")
  , (TDoubleColon, "::")
  , (TInt, "Int")
  -- , (TArrow, "->")
  -- , (TBackslash, "\\")
  , (TComma, ",")
  ]

-- | A parser without user state that works on strings.
type CharParser = P.Parsec String ()

pTokens :: CharParser [ExprToken]
pTokens = many (P.choice [pStaticToken, pInt, pSpace]) <* P.eof

pStaticToken :: CharParser ExprToken
pStaticToken = P.choice $ map (\(tok, syn) -> tok <$ P.string syn) statics

pSpace :: CharParser ExprToken
pSpace = TSpace <$> some (P.oneOf " \n\r\t\f")

pInt :: CharParser ExprToken
pInt = TIntLit 0 <$ P.char '0'
   <|> (\n -> TIntLit . read . (n:)) <$> P.oneOf ['1'..'9'] <*> many (P.oneOf ['0'..'9'])
