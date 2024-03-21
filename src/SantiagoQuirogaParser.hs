module SantiagoQuirogaParser(Expression (..), parseExpr) where

data Expression
  = Var Char
  | Not Expression
  | And Expression Expression
  | Or Expression Expression
  deriving (Show)

parseExpr :: String -> Expression
parseExpr = fst . parseOr . filter (/= ' ')
  where
    parseOr :: String -> (Expression, String)
    parseOr s =
      let (e1, r1) = parseAnd s
       in case r1 of
            ('∨' : r2) ->
              let (e2, r3) = parseOr r2
               in (Or e1 e2, r3)
            _ -> (e1, r1)

    parseAnd :: String -> (Expression, String)
    parseAnd s =
      let (e1, r1) = parseNot s
       in case r1 of
            ('∧' : r2) ->
              let (e2, r3) = parseAnd r2
               in (And e1 e2, r3)
            _ -> (e1, r1)

    parseNot :: String -> (Expression, String)
    parseNot ('~' : r) =
      let (e, r') = parseNot r
       in (Not e, r')
    parseNot ('(' : r) =
      let (e, r') = parseOr r
          (_ : r'') = r'
       in (e, r'')
    parseNot s = parseVar s

    parseVar :: String -> (Expression, String)
    parseVar (c : r)
      | c `elem` ['A', 'B', 'C'] = (Var c, r)
      | otherwise = error ("Invalid character: " ++ [c])
    parseVar [] = error "Empty expression"

main :: IO ()
main = do
  userInput <- getLine
  let expression = parseExpr userInput
  print expression