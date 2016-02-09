module Parsing where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

import Types
import Keywords

separator :: Parser String
separator = (string ",") <|> (string "and")

terminator :: Parser ()
terminator = do
             oneOf "."
             choice [eof , skipMany1 (oneOf " ")]

titleCase :: Parser String
titleCase = do
              first <- upper
              rest <- many (letter)
              let atom = first:rest
              return atom

article :: Parser String
article = (string "a ") <|> (string "the ") <|> (string "The ") <|> (string "A ")


readExpr :: String -> String
readExpr input = case parse parseStatements "ShowStuff" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)


test = readExpr
type TokenSeq = [Token]

data Token = Atom String
             | String String
             | Number Integer
             | Symbol String
             | Separator
             | Error
             | SayOperator
             | AssignOperator
             | MethodDecOp String
             | UnboundVariable String
             | ExprSeq [Token]
             | OutputAsciiOp
             | CombineOp
             | TransferOp
             | RepeatOp
             deriving (Show, Eq)

spaces :: Parser ()
spaces = skipMany1 space

parseSeparator :: Parser Token
parseSeparator = do
                   detected <- separator
                   return Separator

parseTerminator :: Parser ()
parseTerminator = skipMany1 terminator


parseString :: Parser Token
parseString = do
                char '\''
                x <- many (noneOf "\'")
                char '\''
                return $ String x

parseAtom :: Parser Token
parseAtom = do
              first <- letter
              rest <- many (letter)
              let atom = first:rest
              return $ Atom atom

parseArticleSymbol :: Parser Token
parseArticleSymbol = do
                first <- skipMany1 (article)
                rest <- many (letter)
                return $ Symbol rest

parseTitleSymbol :: Parser Token
parseTitleSymbol = do
                val <- titleCase
                return $ Symbol val

parseSymbol :: Parser Token
parseSymbol = parseArticleSymbol <|> parseTitleSymbol


parseMethodDec :: Parser Token
parseMethodDec = do
                name <- many (letter)
                stuff <- string " is where"
                return $ MethodDecOp name

parseNumber :: Parser Token
parseNumber = liftM (Number . read) $ many1 digit

parseSymbolOrAtom :: Parser Token
parseSymbolOrAtom = choice [parseSymbol, parseAtom]

parseSayOperator :: Parser Token
parseSayOperator = do
                first <- choice (map (try . string) sayKeywords)
                return $ SayOperator


parseUnboundVariable :: Parser Token
parseUnboundVariable = do
                first <- choice (map (try . string) ["someone", "they", "them"])
                return $ UnboundVariable "someone"

parseAssignOperator :: Parser Token
parseAssignOperator = do
                first <- choice (map (try . string) assignKeywords)
                return $ AssignOperator

parseOutputAscii:: Parser Token
parseOutputAscii = do
                first <- choice (map (try . string) ["spew", "output", "vomit", "spewed", "vomitted"])
                return $ OutputAsciiOp


parseTransfer:: Parser Token
parseTransfer = do
                first <- choice (map (try . string) ["hugs", "hug"])
                return $ TransferOp


parseCombine:: Parser Token
parseCombine = do
                first <- choice (map (try . string) ["listens to", "listen to", "listened to"])
                return $ CombineOp


parseRepeat:: Parser Token
parseRepeat = do
                first <- choice (map (try . string) ["always"])
                return $ RepeatOp


parseEmptyString:: Parser Token
parseEmptyString = do
                first <- choice (map (try . string) ["nothing"])
                return $ String ""


parseExpr :: Parser Token
parseExpr = try parseMethodDec
         <|> try parseCombine
         <|> try parseEmptyString
         <|> try parseRepeat
         <|> try parseUnboundVariable
         <|> try parseOutputAscii
         <|> try parseSeparator
         <|> try parseSymbol
         <|> try parseSayOperator
         <|> try parseAssignOperator
         <|> try parseAssignOperator
         <|> try parseAtom
         <|> parseString
         <|> parseNumber


parseExprs :: Parser [Token]
parseExprs = sepBy parseExpr (optional space)

parseStatements :: Parser [[Token]]
parseStatements = endBy1 parseExprs terminator

tokenize :: String -> [[Token]]
tokenize input = case parse parseStatements "ShowStuff" input of
    Left err -> [[Error]]
    Right val -> val
