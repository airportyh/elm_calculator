module Calculator exposing (..)
import Evaluator exposing (eval)
import Parser exposing (parse)
import Tokenizer exposing (tokenize)

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

output : String
output =
  List.map (\input -> input ++ " => " ++ (evaluate input)) inputs |>
  String.join "\n"
