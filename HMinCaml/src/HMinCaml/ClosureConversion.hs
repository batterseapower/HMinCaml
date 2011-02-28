-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.ClosureConversion
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module HMinCaml.ClosureConversion(Syntax(..), Closure(..), Function(..), FunctionBody(..), Program(..), closureConvert) where

import HMinCaml.Common
import qualified HMinCaml.InferTypes as I

import List(delete, union, (\\))
import Control.Monad.Fix(MonadFix)
import Control.Monad.State(StateT, runStateT, modify)
import Control.Monad.Trans(lift)

closureConvert :: I.Program -> CompilerError Program
closureConvert (I.Program { I.returnType = t, I.body = e }) = (lift . (evalProgramBuilderT t) . closureConvert') e

data Syntax = Unit
            | IConstant Integer 
            | FConstant Double
            | BConstant Bool 
            | Tuple [TypedIdentifier]
            | Variable TypedIdentifier
            | INegate TypedIdentifier
            | IBinaryExpression IBinaryOperator TypedIdentifier TypedIdentifier
            | FNegate TypedIdentifier
            | FBinaryExpression FBinaryOperator TypedIdentifier TypedIdentifier
            | BNot TypedIdentifier
            | BEqual TypedIdentifier TypedIdentifier
            | BLessEqual TypedIdentifier TypedIdentifier
            | If TypedIdentifier Syntax Syntax
            | LetVal TypedIdentifier Syntax Syntax
            | LetTuple [TypedIdentifier] TypedIdentifier Syntax
            | LetNewClosure TypedIdentifier Closure Syntax
            | ApplyClosure TypedIdentifier TypedIdentifier
            deriving (Eq, Show)
            
   
data Closure = Closure { function :: TypedIdentifier, closeVariables :: [TypedIdentifier] }
             deriving (Eq, Show)

data FunctionBody = FunctionBody { returnType :: Type, expression :: Syntax }
                  deriving (Eq, Show)

data Function = Function { closure :: [TypedIdentifier], argument :: TypedIdentifier, body :: FunctionBody }
              deriving (Eq, Show)

type Functions = [(TypedIdentifier, Function)]
data Program = Program Functions FunctionBody
             deriving (Eq, Show)

type ProgramBuilderT m a = StateT Functions m a

evalProgramBuilderT :: (Monad m) => Type -> ProgramBuilderT m Syntax -> m Program
evalProgramBuilderT t b = do
  (e, fs) <- runStateT b []
  return $ Program fs (FunctionBody { returnType = t, expression = e })

addFunction :: (Monad m) => TypedIdentifier -> Function -> ProgramBuilderT m ()
addFunction x f = do
  modify ((x, f) :)


freeVariables :: I.Syntax -> [TypedIdentifier]
freeVariables (I.Tuple xs) = xs
freeVariables (I.Variable x) = [x]
freeVariables (I.INegate x) = [x]
freeVariables (I.IBinaryExpression op x1 x2) = [x1, x2]
freeVariables (I.FNegate x) = [x]
freeVariables (I.FBinaryExpression op x1 x2) = [x1, x2]
freeVariables (I.BNot x) = [x]
freeVariables (I.BEqual x1 x2) = [x1, x2]
freeVariables (I.BLessEqual x1 x2) = [x1, x2]
freeVariables (I.If xt e1 e2) = xt : (freeVariables e1 `union` freeVariables e2)
freeVariables (I.LetVal x ev eb) = (freeVariables ev `union` freeVariables eb) \\ [x]
freeVariables (I.LetTuple xs xv eb) = ([xv] `union` freeVariables eb) \\ xs
freeVariables (I.LetRec x xas ev eb) = (freeVariables ev `union` freeVariables eb) \\ (x : xas)
freeVariables (I.Apply x1 x2) = [x1, x2]
freeVariables _ = []

closureConvert' :: I.Syntax -> ProgramBuilderT Compiler Syntax
closureConvert' I.Unit = return Unit
closureConvert' (I.IConstant c) = return $ IConstant c
closureConvert' (I.FConstant c) = return $ FConstant c
closureConvert' (I.BConstant c) = return $ BConstant c
closureConvert' (I.Tuple xs) = return $ Tuple xs
closureConvert' (I.Variable x) = return $ Variable x
closureConvert' (I.INegate x) = return $ INegate x
closureConvert' (I.IBinaryExpression op x1 x2) = return $ IBinaryExpression op x1 x2
closureConvert' (I.FNegate x) = return $ FNegate x
closureConvert' (I.FBinaryExpression op x1 x2) = return $ FBinaryExpression op x1 x2
closureConvert' (I.BNot x) = return $ BNot x
closureConvert' (I.BEqual x1 x2) = return $ BEqual x1 x2
closureConvert' (I.BLessEqual x1 x2) = return $ BLessEqual x1 x2
closureConvert' (I.If xt e1 e2) = do
  e1' <- closureConvert' e1
  e2' <- closureConvert' e2
  return $ If xt e1' e2'
closureConvert' (I.LetVal x ev eb) = do
  ev' <- closureConvert' ev
  eb' <- closureConvert' eb
  return $ LetVal x ev' eb'
closureConvert' (I.LetTuple xs xv eb) = do
  eb' <- closureConvert' eb
  return $ LetTuple xs xv eb'
closureConvert' (I.LetRec xf xas ev eb) = mdo
  topLevelID <- convertCurriedFunction xas (typeOf xf) topLevelClosure topLevelID
  eb' <- closureConvert' eb
  return $ LetNewClosure xf (Closure { function = topLevelID, closeVariables = topLevelClosure }) eb'
  
  where
    topLevelClosure = (freeVariables ev \\ (xf : xas))
  
    uniqueTypedIdentifier :: Type -> ProgramBuilderT Compiler TypedIdentifier
    uniqueTypedIdentifier t = do
      x <- uniqueIdentifier
      return $ TypedIdentifier x t
  
    convertCurriedFunction :: [TypedIdentifier] -> Type -> [TypedIdentifier] -> TypedIdentifier -> ProgramBuilderT Compiler TypedIdentifier
    convertCurriedFunction [] _ _ _ = fail "Tried to convert function of no arguments"
    convertCurriedFunction [xa] tf@(FunctionType _ tr) closedVariables topLevelID = do 
      ev' <- closureConvert' ev
      let 
        e = if xf `elem` (freeVariables ev)
            then LetNewClosure xf (Closure { function = topLevelID, closeVariables = topLevelClosure }) ev'
            else ev'
      addCurriedFunction xa tf closedVariables e
    convertCurriedFunction (xa:xas) tf@(FunctionType _ tr) closedVariables topLevelID = do
      previousID <- convertCurriedFunction xas tr (xa : closedVariables) topLevelID
      tempVariable <- uniqueTypedIdentifier tr
      let
        e = LetNewClosure tempVariable (Closure { function = previousID, closeVariables = (xa : closedVariables) }) (Variable tempVariable)
      addCurriedFunction xa tf closedVariables e
  
    addCurriedFunction xa tf@(FunctionType _ tr) closedVariables e = do
      xf <- uniqueTypedIdentifier tf
      let function = Function { closure = closedVariables, argument = xa, 
                                body = FunctionBody { returnType = tr, expression = e } }
      addFunction xf function
      return xf  
closureConvert' (I.Apply x1 x2) = return $ ApplyClosure x1 x2