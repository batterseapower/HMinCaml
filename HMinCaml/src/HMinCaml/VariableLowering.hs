-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.VariableLowering
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module HMinCaml.VariableLowering(Syntax(..), Closure(..), Function(..), Functions, Program(..), VariableReference(..), VariableIndex, lowerVariables) where

import HMinCaml.Common
import qualified HMinCaml.ClosureConversion as C

import Control.Monad(liftM, liftM2, liftM3)
import Control.Monad.State(State, MonadState, runState, get, modify)
import Control.Monad.Trans(lift)

lowerVariables :: C.Program -> CompilerError Program
lowerVariables (C.Program functions function) = return $ Program (map lowerFunctionVariables functions) 
                                                                 (makeFunctionBody [] UnitType [] function)
  where
    makeFunctionBody :: KnownVariables -> Type -> [Type] -> C.FunctionBody -> Function
    makeFunctionBody knownVariables argument closure body = 
      let (locals, expression) = runVariableLowerer knownVariables (lowerVariables' (C.expression body))
      in Function { closure = closure, locals = locals, argument = argument, returnType = C.returnType body, expression = expression }
  
    lowerFunctionVariables :: (TypedIdentifier, C.Function) -> (Identifier, Function)
    lowerFunctionVariables (TypedIdentifier xf _, function) = 
      let closure' = map typeOf (C.closure function)
          knownVariables = (C.argument function, ArgumentVariable) : (zipWith (\var index -> (var, ClosureVariable index)) (C.closure function) [0..])
      in (xf, makeFunctionBody knownVariables (typeOf (C.argument function)) closure' (C.body function))

data Syntax = Unit
            | IConstant Integer 
            | FConstant Double
            | BConstant Bool 
            | Tuple [VariableReference]
            | Variable VariableReference
            | INegate VariableReference
            | IBinaryExpression IBinaryOperator VariableReference VariableReference
            | FNegate VariableReference
            | FBinaryExpression FBinaryOperator VariableReference VariableReference
            | BNot VariableReference
            | BEqual VariableReference VariableReference
            | BLessEqual VariableReference VariableReference
            | If VariableReference Syntax Syntax
            | LetVal VariableReference Syntax Syntax
            | LetTuple [VariableReference] VariableReference Syntax
            | LetNewClosure VariableReference Closure Syntax
            | ApplyClosure (Type,VariableReference) VariableReference
            deriving (Eq, Show)
            
type VariableIndex = Integer
data VariableReference = LocalVariable VariableIndex
                       | ClosureVariable VariableIndex
                       | ArgumentVariable
                       deriving (Eq, Show)
            
data Closure = Closure { function :: Identifier, closeVariables :: [VariableReference] }
             deriving (Eq, Show)

data Function = Function { closure :: [Type], locals :: [Type], argument :: Type, returnType :: Type, expression :: Syntax }
              deriving (Eq, Show)

type Functions = [(Identifier, Function)]              
data Program = Program Functions Function
             deriving (Eq, Show)
             
type KnownVariables = [(TypedIdentifier, VariableReference)]
             
data VariableLowererState = VariableLowererState { knownVariables :: KnownVariables,
                                                   localVariables :: [Type],
                                                   nextLocalIndex :: VariableIndex }
newtype VariableLowerer a = VariableLowerer (State VariableLowererState a)
                          deriving (Monad, MonadState VariableLowererState)
         
runVariableLowerer :: KnownVariables -> VariableLowerer a -> ([Type], a)
runVariableLowerer knownVariables (VariableLowerer e) = 
  let (result, state) = runState e (VariableLowererState { knownVariables = knownVariables,
                                                           localVariables = [],
                                                           nextLocalIndex = 0 })
  in ((reverse . localVariables) state, result)
         
addLocalVariable :: TypedIdentifier -> VariableLowerer VariableReference
addLocalVariable x = do
  nextIndex <- get >>= (return . nextLocalIndex)
  modify (\state -> VariableLowererState { knownVariables = (x, LocalVariable nextIndex) : (knownVariables state),
                                           localVariables = (typeOf x) : (localVariables state),
                                           nextLocalIndex = nextIndex + 1 })
  return $ LocalVariable nextIndex
          
lookupReference :: TypedIdentifier -> VariableLowerer VariableReference
lookupReference x = do
  known <- get >>= (return . knownVariables)
  case lookup x known of
    Just xr -> return xr
    Nothing -> error "Internal compiler error: unknown variable encountered in variable lowering"
             
lowerVariables' :: C.Syntax -> VariableLowerer Syntax
lowerVariables' e =
  case e of
    C.Unit -> return $ Unit
    C.IConstant c -> return $ IConstant c
    C.FConstant c -> return $ FConstant c
    C.BConstant c -> return $ BConstant c
    C.Tuple xs -> liftM Tuple (mapM lookupReference xs)
    C.Variable x -> liftM Variable (lookupReference x)
    C.INegate x -> liftM INegate (lookupReference x)
    C.IBinaryExpression op x1 x2 -> liftM2 (IBinaryExpression op) (lookupReference x1) (lookupReference x2)
    C.FNegate x -> liftM FNegate (lookupReference x)
    C.FBinaryExpression op x1 x2 -> liftM2 (FBinaryExpression op) (lookupReference x1) (lookupReference x2)
    C.BNot x -> liftM BNot (lookupReference x)
    C.BEqual x1 x2 -> liftM2 BEqual (lookupReference x1) (lookupReference x2)
    C.BLessEqual x1 x2 -> liftM2 BLessEqual (lookupReference x1) (lookupReference x2)
    C.If xt e1 e2 -> liftM3 If (lookupReference xt) (lowerVariables' e1) (lowerVariables' e2)
    C.LetVal x ev eb -> liftM3 LetVal (addLocalVariable x) (lowerVariables' ev) (lowerVariables' eb)
    C.LetTuple xs xv eb -> liftM3 LetTuple (mapM addLocalVariable xs) (lookupReference xv) (lowerVariables' eb)
    C.LetNewClosure xf closure eb -> liftM3 LetNewClosure (addLocalVariable xf) (lowerClosureVariables closure) (lowerVariables' eb)
    C.ApplyClosure x1 x2 -> liftM2 ApplyClosure (lookupReference x1 >>= (\r -> return (typeOf x1, r))) (lookupReference x2)
  where
    lowerClosureVariables :: C.Closure -> VariableLowerer Closure
    lowerClosureVariables (C.Closure { C.function = (TypedIdentifier xf _), C.closeVariables = xas }) = do
      variables' <- mapM lookupReference xas
      return $ Closure { function = xf, closeVariables = variables' }