module Calculator exposing (..)

import Char exposing (isDigit)
inputs : List String
inputs = [
  -- "1 + 1",
  -- "5 - 2",
  "3 * 7",
  "5 * 3 * 2",
  -- "9 / 3",
  "12 + 45",
  -- "45 / 982 + 123",
  "4 * 5 + 8",
  "4 + 5 * 8"
  -- "123"
  ]

evaluate : String -> String
evaluate =
  tokenize >>
  parse >>
  eval >>
  toString

eval : Result String (Expr, List Token) -> Int
eval result =
  case result of
    Ok (term, []) -> evalExpr term
    _ -> -999

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

-- type NodeType = Expr | Expr2 | Term | Term2 | Factor | Expr3 | Number Int

type Expr = Expr Term Expr_
type Expr_ = Expr_PlusTerm Term Expr_ | Expr_Epsilon
type Term = Term Factor Term_
type Term_ = Term_TimesFactor Factor Term_ | Term_Epsilon
type Factor = FactorParenthesized Expr | FactorNumber Int

parse : List Token -> Result String (Expr, List Token)
parse = parseExpr

parseExpr : List Token -> Result String (Expr, List Token)
parseExpr tokens =
  case parseTerm tokens of
    Ok (term, moreTokens) ->
      case parseExpr_ moreTokens of
        Ok (expr_, moreTokens) -> Ok (Expr term expr_, moreTokens)
        Err err -> Err err
    Err err -> Err err

parseExpr_ : List Token -> Result String (Expr_, List Token)
parseExpr_ tokens =
  case tokens of
    PlusToken::moreTokens ->
      case parseTerm moreTokens of
        Ok (term, yetMoreTokens) ->
          case parseExpr_ yetMoreTokens of
            Ok (expr_, yetYetMoreTokens) ->
              Ok (Expr_PlusTerm term expr_, yetYetMoreTokens)
            Err err -> Err err
        Err err -> Err err
    _ -> Ok (Expr_Epsilon, tokens)

parseTerm : List Token -> Result String (Term, List Token)
parseTerm tokens =
  case parseFactor tokens of
    Ok (factor, moreTokens) ->
      case parseTerm_ moreTokens of
        Ok (term_, yetMoreTokens) -> Ok (Term factor term_, yetMoreTokens)
        Err err -> Err err
    Err err -> Err err

parseFactor : List Token -> Result String (Factor, List Token)
parseFactor tokens =
  case tokens of
    (NumToken num)::moreTokens -> Ok (FactorNumber num, moreTokens)
    _ -> Err "Not a factor"

parseTerm_ : List Token -> Result String (Term_, List Token)
parseTerm_ tokens =
  case tokens of
    TimesToken::moreTokens ->
      case parseFactor moreTokens of
        Ok (factor, yetMoreTokens) ->
          case parseTerm_ yetMoreTokens of
            Ok (term_, yetYetMoreTokens) ->
              Ok (Term_TimesFactor factor term_, yetYetMoreTokens)
            Err err -> Err err
        Err err -> Err err
    _ -> Ok (Term_Epsilon, tokens)

-- displayResult : ParserResult -> String
-- displayResult parserResult =
--   case parserResult of
--     Ok expr -> toString (eval expr)
--     Err err -> err

evalExpr : Expr -> Int
evalExpr expr =
  case expr of
    Expr term expr_ -> evalExpr_ (evalTerm term) expr_

evalExpr_ : Int -> Expr_ -> Int
evalExpr_ num expr_ =
  case expr_ of
    Expr_Epsilon -> num
    Expr_PlusTerm term expr_2 -> evalExpr_ ((evalTerm term) + num) expr_2

evalTerm : Term -> Int
evalTerm term =
  case term of
    Term factor term_ -> evalTerm_ (evalFactor factor) term_

evalTerm_ : Int -> Term_ -> Int
evalTerm_ num term_ =
  case term_ of
    Term_Epsilon -> num
    Term_TimesFactor factor term_2 -> evalTerm_ ((evalFactor factor) * num) term_2

evalFactor : Factor -> Int
evalFactor factor =
  case factor of
    FactorNumber num -> num
    _ -> -999

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
