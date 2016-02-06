module Parsing where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

import Types

separator :: Parser Char
separator = oneOf ","

terminator :: Parser ()
terminator = do
             oneOf "."
             choice [eof , skipMany1 (oneOf " ")]

readExpr :: String -> String
readExpr input = case parse parseStatements "ShowStuff" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)


test = readExpr

data Token = Atom String
             | String String
             | Number Integer
             | Symbol String
             | Seq [Token]
             | Separator
             | Error
             deriving Show

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

titleCase = do
               first <- upper
               rest <- many (letter)
               let atom = first:rest
               return atom

article = (string "a ") <|> (string "the ") <|> (string "The ")

parseArticleSymbol :: Parser Token
parseArticleSymbol = do
                first <- skipMany1 (article)
                rest <- many (letter)
                return $ Symbol rest

parseTitleSymbol :: Parser Token
parseTitleSymbol = do
                val <- titleCase
                return $ Symbol val

parseSymbol = parseArticleSymbol <|> parseTitleSymbol


parseNumber :: Parser Token
parseNumber = liftM (Number . read) $ many1 digit

parseSymbolOrAtom = choice [parseSymbol, parseAtom]

parseExpr :: Parser Token
parseExpr = try parseSymbol
         <|> parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseSeparator

-- parseSeq :: Parser [Token]
parseSeq = sepBy parseExpr (optional space)

parseStatements = endBy1 parseSeq terminator
