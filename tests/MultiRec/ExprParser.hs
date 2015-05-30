module MultiRec.ExprParser where

import Annotations.Bounds
import Annotations.BoundsParser
import MultiRec.ExprLexer
import MultiRec.Expr
import Annotations.MultiRec.ErrorAlg
import Annotations.MultiRec.ParserCombinators
import Annotations.Except
import Annotations.MultiRec.Annotated
import Annotations.MultiRec.Yield
import Annotations.MultiRec.Any

import qualified Text.Parsec as P

import Data.Maybe
import Control.Applicative
import Control.Monad.Identity

import Debug.Trace


type ExprParser = YP ExprToken Tuples Identity

pExpr :: ExprParser Expr
pExpr = do
  left <- getPos
  ex <- pAdd
  P.option ex $ do
    pToken TDoubleColon
    ty <- pType
    mkBounded Expr left (ETyped ex ty)

pAdd :: ExprParser Expr
pAdd = chainl Expr pMul (EAdd <$ pToken TPlus)

pMul :: ExprParser Expr
pMul = chainl Expr pFactor (EAdd <$ pToken TStar)

pFactor :: ExprParser Expr
pFactor = pIntLit <|> pTupleVal

pIntLit :: ExprParser Expr
pIntLit = unit Expr $ (\(TIntLit n) -> EIntLit n) <$> satisfy isIntLit

pTupleVal :: ExprParser Expr
pTupleVal = pTuple Expr pExpr ETup

pType :: ExprParser Type
pType = pTyInt <|> pTyTuple

pTuple :: Tuples ix -> ExprParser ix -> (ix -> ix -> ix) -> ExprParser ix
pTuple w pEl f = do
  left <- getPos
  pToken TLParen
  lhs <- pEl
  ty <- P.option lhs $ do
    pToken TComma
    rhs <- pEl
    mkBounded w left (f lhs rhs)
  pToken TRParen
  return ty

pTyTuple :: ExprParser Type
pTyTuple = pTuple Type pType TyTup

pTyInt :: ExprParser Type
pTyInt = unit Type $ TyInt <$ pToken TInt

readExpr :: String -> AnnFix Bounds Tuples Expr
readExpr input = case P.runParser pTokens () "" input of
  Left msg -> error (show msg)
  Right toks ->
    let toks' = trace (show toks) $ collapse isSpace toks
        leftmost = leftMargin $ snd $ head toks'
        p = P.runParserT (pExpr <* P.eof) leftmost "" toks'
     in case runYieldG p of
       (Left msg, _) -> error (show msg)
       (Right _, Just expr) -> fromJust (matchAnyF Expr expr)

inferExprType :: String -> Except [(String, Bounds)] Type
inferExprType = errorCata (mkErrorAlg inferType) Expr . readExpr
