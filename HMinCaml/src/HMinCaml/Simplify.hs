-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.Normalize
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module HMinCaml.Simplify(Syntax(..), simplify) where

import HMinCaml.Common
import qualified HMinCaml.Parser as P

simplify :: P.Syntax -> CompilerError Syntax
simplify = return . simplify'

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
            | BEqual Syntax Syntax
            | BLessEqual Syntax Syntax
            | If Syntax Syntax Syntax
            | LetVal Identifier Syntax Syntax
            | LetTuple [Identifier] Syntax Syntax
            | LetRec Identifier [Identifier] Syntax Syntax
            | Apply Syntax Syntax
            deriving (Eq, Show)
            
-- Makes initial transformations on the syntax tree to simplify it
simplify' :: P.Syntax -> Syntax
simplify' P.Unit = Unit
simplify' (P.IConstant c) = IConstant c
simplify' (P.FConstant c) = FConstant c
simplify' (P.BConstant c) = BConstant c
simplify' (P.Tuple es) = Tuple (map simplify' es)
simplify' (P.Variable x) = Variable x
simplify' (P.INegate e) = INegate (simplify' e)
simplify' (P.IBinaryExpression op e1 e2) = IBinaryExpression op (simplify' e1) (simplify' e2)
simplify' (P.FNegate e) = FNegate (simplify' e)
simplify' (P.FBinaryExpression op e1 e2) = FBinaryExpression op (simplify' e1) (simplify' e2)
simplify' (P.BNot e) = BNot (simplify' e)
simplify' (P.BBinaryExpression Less e1 e2) = BNot (BLessEqual (simplify' e2) (simplify' e1))	 --
simplify' (P.BBinaryExpression LessEqual e1 e2) = BLessEqual (simplify' e1) (simplify' e2)		 -- These five lines contain the
simplify' (P.BBinaryExpression Greater e1 e2) = BNot (BLessEqual (simplify' e1) (simplify' e2))  -- only real work this stage does
simplify' (P.BBinaryExpression GreaterEqual e1 e2) = BLessEqual (simplify' e2) (simplify' e1)	 --
simplify' (P.BBinaryExpression Equal e1 e2) = BEqual (simplify' e1) (simplify' e2)               --
simplify' (P.If e1 e2 e3) = If (simplify' e1) (simplify' e2) (simplify' e3)
simplify' (P.LetVal x e1 e2) = LetVal x (simplify' e1) (simplify' e2)
simplify' (P.LetTuple xs e1 e2) = LetTuple xs (simplify' e1) (simplify' e2)
simplify' (P.LetRec x xs e1 e2) = LetRec x xs (simplify' e1) (simplify' e2)
simplify' (P.Apply e1 e2) = Apply (simplify' e1) (simplify' e2)