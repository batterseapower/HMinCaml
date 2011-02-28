-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.Parser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module HMinCaml.Parser(Syntax(..), parse) where

import Text.ParserCombinators.Parsec(GenParser, (<|>), (<?>), try, oneOf, many1, sepBy)

import qualified Text.ParserCombinators.Parsec as Parsec
import qualified Text.ParserCombinators.Parsec.Token as Token

import Text.ParserCombinators.Parsec.Expr(Assoc(..), Operator(..), buildExpressionParser)
import Text.ParserCombinators.Parsec.Language

import HMinCaml.Common

-- Converts a string into an internal syntax tree representation
parse :: String -> CompilerError Syntax
parse s = either (fail . show) return (Parsec.parse parser "" s)

data Syntax = Unit
            | IConstant Integer 
            | FConstant Double
            | BConstant Bool
            | Tuple [Syntax]
            | Variable Identifier
            | INegate Syntax
            | IBinaryExpression IBinaryOperator Syntax Syntax
            | FNegate Syntax
            | FBinaryExpression FBinaryOperator Syntax Syntax
            | BNot Syntax
            | BBinaryExpression BBinaryOperator Syntax Syntax
            | If Syntax Syntax Syntax
            | LetVal Identifier Syntax Syntax
            | LetTuple [Identifier] Syntax Syntax
            | LetRec Identifier [Identifier] Syntax Syntax
            | Apply Syntax Syntax
            deriving (Eq, Show)

-- Lexing stage
mlLanguage = LanguageDef { commentStart = "(*", 
                           commentEnd = "*)",
                           commentLine = "",
                           nestedComments = True,
                           identStart = Parsec.letter <|> Parsec.char '_',
                           identLetter = Parsec.alphaNum <|> Parsec.char '_',
                           opStart = oneOf "+-*/=!<>",
                           opLetter = oneOf ".=",
                           reservedNames = ["let", "rec", "in", "if", "then", "else", "true", "false"],
                           reservedOpNames = ["+", "-", "*", "/", "+.", "-.", "*.", "/.", "!", "<", "<=", ">", ">=", "="],
                           caseSensitive = True }

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser mlLanguage

whiteSpace     = Token.whiteSpace lexer
symbol         = Token.symbol lexer
reserved       = Token.reserved lexer

identifier     = Token.identifier lexer
naturalOrFloat = Token.naturalOrFloat lexer
reservedOp     = Token.reservedOp lexer

comma          = Token.comma lexer
commaSep       = Token.commaSep lexer

-- Parsing stage
parser :: GenParser Char () Syntax
parser = do { whiteSpace; 
              e <- expression; 
              Parsec.eof; 
              return e }

expression = try applicationExpression                  -- Try to get multiple consecutive expression factors
         <|> try operatorExpression                     -- Try to get expression factors seperated by operators
         <|> expressionFactor                           -- OK, looks like we will have to settle for only one expression factor
         <?> "expression"
   

operatorExpression = buildExpressionParser table expressionFactor <?> "arithmetic or boolean expression"
  where
    table = [
             [inOp "!" BNot],
             [inOp "-" INegate,           inOp "-." FNegate],
             [iOp "/" Divide AssocLeft,   fOp "/." Divide AssocLeft],
             [iOp "*" Multiply AssocLeft, fOp "*." Multiply AssocLeft],
             [iOp "+" Add AssocLeft,      fOp "+." Add AssocLeft],
             [iOp "-" Subtract AssocLeft, fOp "-." Subtract AssocLeft],
             [bOp "<" Less AssocLeft,     bOp "<=" LessEqual AssocLeft],
             [bOp ">" Greater AssocLeft,  bOp ">=" GreaterEqual AssocLeft],
             [bOp "=" Equal AssocLeft]
            ] 
    
    skip s f = (do { reservedOp s; return f })
    
    inOp s f = Prefix (skip s f)
    
    iOp s f = op s (IBinaryExpression f)
    fOp s f = op s (FBinaryExpression f)
    bOp s f = op s (BBinaryExpression f)
    op s f assoc = Infix (skip s f) assoc
    
variable = do { x <- identifier; 
                return $ Variable x }
            <?> "variable"

expressionFactor = tupleOrParenthesizedValue
               <|> conditionalExpression
               <|> letExpression
               <|> booleanConstant
               <|> variable
               <|> numericConstant
               <?> "expression factor"

numericConstant = do { number <- naturalOrFloat; 
                       return $ either IConstant FConstant number }
                   <?> "constant"
            
booleanConstant = do { reserved "true"; return $ BConstant True }
              <|> do { reserved "false"; return $ BConstant False }

conditionalExpression = do { reserved "if"; e <- expression; 
                             reserved "then"; eT <- expression; 
                             reserved "else"; eF <- expression; 
                             return $ If e eT eF }
                         <?> "conditional expression"

letLValue :: GenParser Char () (Syntax -> Syntax -> Syntax)
letLValue = do { reserved "rec"; 
                 name <- identifier;
                 args <- sepBy identifier whiteSpace;
                 return $ LetRec name args }
        <|> do { identifiers <- tupleOrParenthesized identifier;
                 return $ (either LetTuple LetVal identifiers) }
        <|> do { name <- identifier;
                 return $ LetVal name }

letExpression = do { reserved "let";
                     lValue <- letLValue;
                     symbol "="; valueExpression <- expression;
                     reserved "in"; body <- expression;
                     return $ lValue valueExpression body }
                 <?> "let expression"

applicationExpression = do { function <- expressionFactor;
                             args <- many1 expressionFactor;
                             return $ foldl Apply function args }
                         <?> "application expression"

tupleOrParenthesizedValue = try unit -- This ambiguity can be resolved but not cleanly
                        <|> do { expressions <- tupleOrParenthesized expression;
                                 return $ either Tuple id expressions }
                        <?> "tuple or parenthesized value"

unit = do { symbol "(";
            symbol ")";
            return Unit }
        <?> "unit"

tupleOrParenthesized subExpression = 
  do { symbol "("; 
       e <- subExpression;
       eTup <- do { comma;
                    es <- commaSep subExpression;
                    return $ Left (e : es) }
           <|> do { return $ Right e };
       symbol ")";
       return eTup }
   <?> "tuple or parenthesized expression"