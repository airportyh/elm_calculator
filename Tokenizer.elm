module Tokenizer exposing (..)
import Char exposing (isDigit)

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
