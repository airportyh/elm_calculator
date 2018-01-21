module Evaluator exposing (..)
import Parser exposing (..)
import Tokenizer exposing (Token)

eval : Result String (Expr, List Token) -> String
eval result =
  case result of
    Ok (expr, []) -> toString (evalExpr expr)
    Ok (expr, moreTokens) -> "Unexpected extra tokens: " ++ (toString moreTokens)
    Err err -> "Error: " ++ err

evalExpr : Expr -> Int
evalExpr expr =
  case expr of
    Expr term expr_ -> (evalExpr_ (evalTerm term) expr_)

evalExpr_ : Int -> Expr_ -> Int
evalExpr_ value expr_ =
  case expr_ of
    Expr_Epsilon -> value
    Expr_Term TermOpPlus term expr_2 ->
      evalExpr_ ((evalTerm term) + value) expr_2
    Expr_Term TermOpMinus term expr_2 ->
      evalExpr_ (value - (evalTerm term)) expr_2

evalTerm : Term -> Int
evalTerm term =
  case term of
    Term factor term_ -> (evalTerm_ (evalFactor factor) term_)

evalTerm_ : Int -> Term_ -> Int
evalTerm_ value term_ =
  case term_ of
    Term_Epsilon -> value
    Term_Factor FactorOpTimes factor term_2 ->
      (evalTerm_ ((evalFactor factor) * value) term_2)
    Term_Factor FactorOpDivide factor term_2 ->
      (evalTerm_ (value // (evalFactor factor)) term_2)

evalFactor : Factor -> Int
evalFactor factor =
  case factor of
    FactorNumber num -> num
