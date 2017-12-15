module Calculator exposing (..)

import Char exposing (isDigit)
inputs : List String
inputs = [
  "1 + 1",
  "5 - 2",
  "2 * 4",
  "9 / 3",
  "12 + 45",
  "45 / 982 + 123",
  "4 * 5 + 8",
  "4 + 5 * 8",
  "123"]

evaluate : String -> String
evaluate =
  tokenize >>
  parse >>
  displayResult

type Expr =
  Number Int |
  BinaryOperation OpType1 Expr Expr
  -- BinaryOperation2 OpType2 Expr Expr

type alias ParserResult = Result String Expr

displayResult : ParserResult -> String
displayResult parserResult =
  case parserResult of
    Ok expr -> toString (eval expr)
    Err err -> err

eval : Expr -> Int
eval expr =
  case expr of
    Number n -> n
    BinaryOperation op expr1 expr2 ->
      case op of
        -- Plus -> (eval expr1) + (eval expr2)
        -- Minus -> (eval expr1) - (eval expr2)
        Times -> (eval expr1) * (eval expr2)
        Divide -> (round ((toFloat (eval expr1)) / (toFloat (eval expr2))))


parse : List Token -> ParserResult
parse = parseExpr

parseExpr : List Token -> ParserResult
parseExpr tokens =
  case parseBinaryOperator1 tokens of
    Ok binOp -> Ok binOp
    Err _ -> parseNumber tokens

parseBinaryOperator1 : List Token -> ParserResult
parseBinaryOperator1 tokens =
  case tokens of
    (NumToken num1)::(OpToken1 op)::[NumToken num2] ->
      Ok (BinaryOperation op (Number num1) (Number num2))
    (NumToken num1)::(OpToken1 op1)::NumToken num2::(OpToken1 op2)::more ->
      case parseExpr more of
        Ok rhs ->
          let lhs = BinaryOperation op1 (Number num1) (Number num2)
          in Ok (BinaryOperation op2 lhs rhs)
        Err err -> Err "Fail in parseBinaryOperator1"
    _ -> Err "Fail in parseBinaryOperator1"

-- parseBinaryOperator2 : List Token -> ParserResult
-- parseBinaryOperator2 tokens =
--   case tokens of
--     (NumToken num1)::(OpToken2 op)::[NumToken num2] ->
--       Ok (BinaryOperation op (Number num1) (Number num2))
--     (NumToken num1)::(OpToken2 op1)::NumToken num2::(OpToken2 op2)::more ->
--       case parseExpr more of
--         Ok rhs ->
--           let lhs = BinaryOperation2 op1 (Number num1) (Number num2)
--           in Ok (BinaryOperation2 op2 lhs rhs)
--         Err err -> Err "Fail in parseBinaryOperator2"
--     _ -> Err "Fail in parseBinaryOperator2"

parseNumber : List Token -> ParserResult
parseNumber tokens =
  case tokens of
    [NumToken num] -> Ok (Number num)
    _ -> Err "Fail in parser_number"

type Token =
  OpToken1 OpType1 |
  OpToken2 OpType2 |
  NumToken Int |
  ErrToken String

type OpType1 = Times | Divide
type OpType2 = Plus | Minus

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
        -- '+' -> (Open, OpToken1 Plus :: tokenList)
        -- '-' -> (Open, OpToken1 Minus :: tokenList)
        '*' -> (Open, OpToken1 Times :: tokenList)
        '/' -> (Open, OpToken1 Divide :: tokenList)
        _ -> (Open, ErrToken
          ("Unknown OpToken1 " ++ (String.fromChar chr)) :: tokenList)
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
        -- '+' -> (Open, OpToken1 Plus :: tokenList)
        -- '-' -> (Open, OpToken1 Minus :: tokenList)
        '*' -> (Open, OpToken1 Times :: tokenList)
        '/' -> (Open, OpToken1 Divide :: tokenList)
        _ -> (Open, ErrToken
          ("Unknown OpToken1 " ++ (String.fromChar chr)) :: tokenList)
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
