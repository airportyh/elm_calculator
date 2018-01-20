module Calculator exposing (..)

import Char exposing (isDigit)
import Result exposing (andThen)

inputs : List String
inputs = [
  "1 + 1",
  "5 - 2",
  "3 * 7",
  "5 * 3 * 2",
  "9 / 3",
  "12 + 45",
  "45 / 982 + 123",
  "4 * 5 + 8",
  "4 + 5 * 8",
  "123"
  ]

evaluate : String -> String
evaluate =
  tokenize >>
  parse >>
  eval >>
  toString

eval : Result String (Expr, List Token) -> String
eval result =
  case result of
    Ok (expr, []) -> toString (evalExpr expr)
    Ok (expr, moreTokens) -> "Unexpected extra tokens: " ++ (toString moreTokens)
    Err err -> "Error: " ++ err

-- Grammar
--
-- Expr -> Expr + Term | Term
-- Term -> Term * Factor | Factor
-- F -> Number
--
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

type Token =
  PlusToken |
  MinusToken |
  TimesToken |
  DivideToken |
  NumToken Int |
  ErrToken String

type OpType = Times | Divide | Plus | Minus

type State = Open | CollectingDigits

accumulateToken : Char -> (State, List Token) -> (State, List Token)
accumulateToken chr (state, tokenList) =
  if chr == ' ' then
    (state, tokenList)
  else if state == Open then
    if isDigit chr then
      case (String.toInt (String.fromChar chr)) of
        Ok int -> (CollectingDigits, NumToken int :: tokenList)
        Err err -> (Open, ErrToken err :: tokenList)
    else
      case chr of
        '+' -> (Open, PlusToken :: tokenList)
        '-' -> (Open, MinusToken :: tokenList)
        '*' -> (Open, TimesToken :: tokenList)
        '/' -> (Open, DivideToken :: tokenList)
        _ -> (Open, ErrToken
          ("Unknown OpToken " ++ (String.fromChar chr)) :: tokenList)
  else if state == CollectingDigits then
    if isDigit chr then
      case (String.toInt (String.fromChar chr)) of
        Ok int ->
          case tokenList of
            (NumToken existingInt)::rest ->
              (CollectingDigits, NumToken (existingInt * 10 + int) :: rest)
            _ -> (Open, (ErrToken "Not supposed to happen") :: tokenList)
        Err err -> (Open, ErrToken err :: tokenList)
    else
      case chr of
        '+' -> (Open, PlusToken :: tokenList)
        '-' -> (Open, MinusToken :: tokenList)
        '*' -> (Open, TimesToken :: tokenList)
        '/' -> (Open, DivideToken :: tokenList)
        _ -> (Open, ErrToken
          ("Unknown OpToken " ++ (String.fromChar chr)) :: tokenList)
  else
    (state, tokenList)

tokenize : String -> List Token
tokenize =
  String.toList >>
  List.foldl accumulateToken (Open, []) >>
  Tuple.second >>
  List.reverse

output : String
output =
  List.map (\input -> input ++ " => " ++ (evaluate input)) inputs |>
  String.join "\n"
