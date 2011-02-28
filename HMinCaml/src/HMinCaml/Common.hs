-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.Common
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module HMinCaml.Common where

import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Fix(MonadFix)
import Control.Monad.State(State, evalState, get, modify)
import Control.Monad.Trans(MonadTrans, lift)

type ErrorM a b = ErrorT String a b
type Error a = ErrorM Identity a

runError = runIdentity . runErrorT

type Identifier = String

data Type = UnitType
          | IntegerType
          | FloatingPointType
          | BooleanType
          | TupleType [Type]
          | FunctionType Type Type
          | VariableType Identifier
          deriving (Eq)
         
instance Show Type
  where
    show UnitType = "unit"
    show IntegerType = "integer"
    show FloatingPointType = "float"
    show BooleanType = "boolean"
    show (TupleType ts) = "(" ++ (intersperse ", " (map show ts)) ++ ")"
    show (FunctionType tA tR) = "(" ++ (show tA) ++ " -> " ++ (show tR) ++ ")"
    show (VariableType x) = x ++ "'"

intersperse seperator [] = []
intersperse seperator (x:[]) = x
intersperse seperator (x:xs) = x ++ seperator ++ (intersperse seperator xs)

data TypedIdentifier = TypedIdentifier Identifier Type
                     deriving (Eq)

instance Show TypedIdentifier 
  where
    show (TypedIdentifier x t) = "(" ++ x ++ " :: " ++ (show t) ++ ")"

typeOf (TypedIdentifier _ t) = t
  
data NBinaryOperator = Add
                     | Subtract
                     | Multiply
                     | Divide
                     deriving (Eq, Show)
            
type IBinaryOperator = NBinaryOperator
type FBinaryOperator = NBinaryOperator            
                    
data BBinaryOperator = Less
                     | LessEqual
                     | Greater
                     | GreaterEqual
                     | Equal
                     deriving (Eq, Show)
                     

newtype Compiler a = Compiler (State Integer a)
                   deriving (Monad, MonadFix)

liftState = Compiler

runCompiler :: Compiler a -> a
runCompiler (Compiler s) = evalState s 0

class (Monad m) => MonadCompiler m where
  uniqueIdentifier :: m Identifier
  
instance MonadCompiler Compiler where
  uniqueIdentifier = (liftState (modify (+1))) >> liftState get >>= (return . show)
  
instance (MonadTrans mt, MonadCompiler m, Monad (mt m)) => MonadCompiler (mt m) where
  uniqueIdentifier = lift uniqueIdentifier
  
type CompilerError a = ErrorM Compiler a

runCompilerError :: CompilerError a -> Either String a
runCompilerError = runCompiler . runErrorT