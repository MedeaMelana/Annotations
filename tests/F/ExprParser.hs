{-# LANGUAGE Rank2Types #-}

module F.ExprParser (
    ExprParser, pExpr, parseExpr
  ) where

import F.ExprLexer
import F.Expr

import Annotations.Bounds
import Annotations.BoundsParser
import Annotations.F.Annotated
import Annotations.F.ParserCombinators

import Control.Applicative
import Control.Monad.Identity
import Data.Function
import qualified Text.Parsec as P


-- | A bounds parser that works on 'ExprToken's.
type ExprParser a = forall m. Monad m => P ExprToken m a

-- | Recognises expressions. The expressions are annotated with position information.
pExpr :: ExprParser (AnnFix Bounds ExprF)
pExpr = chainl pTerm (Add <$ pToken TPlus <|> Sub <$ pToken TMinus)

pTerm :: ExprParser (AnnFix Bounds ExprF)
pTerm = chainl pFactor (Mul <$ pToken TStar <|> Div <$ pToken TSlash)

pFactor :: ExprParser (AnnFix Bounds ExprF)
pFactor = pNum <|> pToken TOpen *> pExpr <* pToken TClose

pNum :: ExprParser (AnnFix Bounds ExprF)
pNum = unit $ (\(TNum n) -> Num n) <$> satisfy isNum

-- | Runs 'pTokens' on the input and and 'pExpr' on the resulting tokens.
parseExpr :: String -> Either P.ParseError (AnnFix Bounds ExprF)
parseExpr input = do
  toks <- P.runParser (collapse isSpace <$> pTokens) () "" input
  let startMargin = (leftMargin . snd . head) toks
  P.runParser (pExpr <* P.eof) startMargin "" toks
