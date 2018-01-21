module Parser exposing (..)
import Result exposing (andThen)
import Tokenizer exposing (..)

-- Grammar
-- =======
-- Expr -> Expr + Term | Term
-- Term -> Term * Factor | Factor
-- Factor -> Number
--
-- Non-Left-Recursive Form
-- =======================
-- Expr -> Term Expr_
-- Expr_ -> + Term Expr_ | Epsilon
-- Term -> Factor Term_
-- Term_ -> * Factor Term_ | Epsilon
-- Factor -> ( Expr ) | ID | Number


type Expr = Expr Term Expr_
type Expr_ = Expr_Term TermOp Term Expr_ | Expr_Epsilon
type TermOp = TermOpPlus | TermOpMinus
type Term = Term Factor Term_
type Term_ = Term_Factor FactorOp Factor Term_ | Term_Epsilon
type FactorOp = FactorOpTimes | FactorOpDivide
type Factor = FactorNumber Int

parse : List Token -> Result String (Expr, List Token)
parse = parseExpr

parseExpr : List Token -> Result String (Expr, List Token)
parseExpr tokens =
  (parseTerm tokens)
    |> andThen (\(term, moreTokens) ->
      parseExpr_ moreTokens
      |> andThen (\(expr_, moreTokens) ->
        Ok (Expr term expr_, moreTokens)))

parseExpr_ : List Token -> Result String (Expr_, List Token)
parseExpr_ tokens =
  let parseRest = (\opToken tokens ->
    parseTerm tokens
      |> andThen (\(term, moreTokens) ->
        parseExpr_ moreTokens
        |> andThen (\(expr_, moreTokens) ->
          Ok (Expr_Term opToken term expr_, moreTokens))))
  in
    case tokens of
      PlusToken::moreTokens -> parseRest TermOpPlus moreTokens
      MinusToken::moreTokens -> parseRest TermOpMinus moreTokens
      _ -> Ok (Expr_Epsilon, tokens)

parseTerm : List Token -> Result String (Term, List Token)
parseTerm tokens =
  parseFactor tokens
    |> andThen (\(factor, moreTokens) ->
      parseTerm_ moreTokens
        |> andThen (\(term_, moreTokens) ->
          Ok (Term factor term_, moreTokens)))

parseFactor : List Token -> Result String (Factor, List Token)
parseFactor tokens =
  case tokens of
    (NumToken num)::moreTokens -> Ok (FactorNumber num, moreTokens)
    tok::_ -> Err ("Expected a number here, but got " ++ (toString tok))
    [] -> Err "Expected a number here, but got end of input"

parseTerm_ : List Token -> Result String (Term_, List Token)
parseTerm_ tokens =
  let parseRest = (\opToken tokens ->
    parseFactor tokens
      |> andThen (\(factor, moreTokens) ->
        parseTerm_ moreTokens
          |> andThen (\(term_, moreTokens) ->
            Ok (Term_Factor opToken factor term_, moreTokens))))
  in
    case tokens of
      TimesToken::moreTokens -> parseRest FactorOpTimes moreTokens
      DivideToken::moreTokens -> parseRest FactorOpDivide moreTokens
      _ -> Ok (Term_Epsilon, tokens)
