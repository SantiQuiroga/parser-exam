module Main(main) where

import qualified Data.Map as Map
import qualified SantiagoQuirogaParser as Parser

type VarAssignments = Map.Map Char Bool

evaluate :: Parser.Expression -> VarAssignments -> Bool
evaluate expr vars = case expr of
  Parser.Var c -> Map.findWithDefault False c vars
  Parser.Not e -> not $ evaluate e vars
  Parser.And e1 e2 -> evaluate e1 vars && evaluate e2 vars
  Parser.Or e1 e2 -> evaluate e1 vars || evaluate e2 vars

main :: IO ()
main = do
  putStrLn "Enter the expression:"
  userInput <- getLine
  let expr = Parser.parseExpr userInput
  let vars = Map.fromList [('A', True), ('B', False), ('C', True)]
  let result = evaluate expr vars
  print result
