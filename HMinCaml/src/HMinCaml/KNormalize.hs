-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.KNormalize
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

{-# OPTIONS_GHC -fglasgow-exts #-}

module HMinCaml.KNormalize where

import HMinCaml.Common
import qualified HMinCaml.Simplify as S

import Data.Foldable(foldrM)
import Control.Monad(mapM)
import Control.Monad.Error(runErrorT)
import Control.Monad.State(State, evalState, modify, get)

-- Assign a name to every sub expression so that we can reason about them
kNormalize :: S.Syntax -> CompilerError Syntax
kNormalize e = either fail return (evalKNormalizer (kNormalize' [] e))

data Syntax = Unit
            | IConstant Integer 
            | FConstant Double
            | BConstant Bool 
            | Tuple [Identifier]
            | Variable Identifier
            | INegate Identifier
            | IBinaryExpression IBinaryOperator Identifier Identifier
            | FNegate Identifier
            | FBinaryExpression FBinaryOperator Identifier Identifier
            | BNot Identifier
            | BEqual Identifier Identifier
            | BLessEqual Identifier Identifier
            | If Identifier Syntax Syntax
            | LetVal Identifier Syntax Syntax
            | LetTuple [Identifier] Identifier Syntax
            | LetRec Identifier [Identifier] Syntax Syntax
            | Apply Identifier Identifier
            deriving (Eq, Show)
            
type KNormalizer a = ErrorM (State Integer) a
            
evalKNormalizer :: KNormalizer a -> Either String a
evalKNormalizer = (flip evalState 0) . runErrorT
            

type ScopeBinding = (Identifier, Identifier)
type ScopeBindings = [ScopeBinding]
            
bind :: ScopeBinding -> ScopeBindings -> ScopeBindings
bind = (:)
          
bindAll :: ScopeBindings -> ScopeBindings -> ScopeBindings
bindAll = (++)
            
uniqueVariable :: Identifier -> KNormalizer (Identifier, ScopeBinding)
uniqueVariable x = do
  x' <- uniqueTempVariable
  let x'' = x ++ "." ++ x'
  return (x'', (x, x''))
            
uniqueTempVariable :: KNormalizer Identifier
uniqueTempVariable = (modify (+1)) >> get >>= (return . show)
            
unaryExpression bs operator e = do
  x <- uniqueTempVariable
  eN <- kNormalize' bs e
  return $ LetVal x eN (operator x)
            
binaryExpression bs operator e1 e2 = do
  x1 <- uniqueTempVariable
  x2 <- uniqueTempVariable
  e1N <- kNormalize' bs e1
  e2N <- kNormalize' bs e2
  return $ LetVal x1 e1N (LetVal x2 e2N (operator x1 x2))
            
kNormalize' :: ScopeBindings -> S.Syntax -> KNormalizer Syntax
kNormalize' _ S.Unit = return Unit
kNormalize' _ (S.IConstant c) = return $ IConstant c
kNormalize' _ (S.FConstant c) = return $ FConstant c
kNormalize' _ (S.BConstant c) = return $ BConstant c
kNormalize' bs (S.Tuple es) = do
  xs <- mapM (const uniqueTempVariable) es
  foldrM (\(x, e) e' -> do { eN <- kNormalize' bs e; return $ LetVal x eN e' }) (Tuple xs) (zip xs es)
kNormalize' bs (S.Variable x) = do
  x' <- case (lookup x bs) of
          Just x' -> return x'
          Nothing -> fail $ "Unbound variable '" ++ x ++ "'"
  return $ Variable x'
kNormalize' bs (S.INegate e) = unaryExpression bs INegate e  
kNormalize' bs (S.IBinaryExpression op e1 e2) = binaryExpression bs (IBinaryExpression op) e1 e2
kNormalize' bs (S.FNegate e) = unaryExpression bs FNegate e  
kNormalize' bs (S.FBinaryExpression op e1 e2) = binaryExpression bs (FBinaryExpression op) e1 e2
kNormalize' bs (S.BNot e) = unaryExpression bs BNot e
kNormalize' bs (S.BEqual e1 e2) = binaryExpression bs BEqual e1 e2
kNormalize' bs (S.BLessEqual e1 e2) = binaryExpression bs BLessEqual e1 e2
kNormalize' bs (S.If et e1 e2) = do
  xt <- uniqueTempVariable
  etN <- kNormalize' bs et
  e1N <- kNormalize' bs e1
  e2N <- kNormalize' bs e2
  return $ LetVal xt etN (If xt e1N e2N)
kNormalize' bs (S.LetVal x ev eb) = do
  (x', b) <- uniqueVariable x
  evN <- kNormalize' bs ev
  ebN <- kNormalize' (b `bind` bs) eb
  return $ LetVal x' evN ebN
kNormalize' bs (S.LetTuple xs ev eb) = do
  newVariables <- mapM uniqueVariable xs
  let (xs', bs') = unzip newVariables
  xv <- uniqueTempVariable
  evN <- kNormalize' bs ev
  ebN <- kNormalize' (bs' `bindAll` bs) eb
  return $ LetVal xv evN (LetTuple xs' xv ebN)
kNormalize' bs (S.LetRec xf xas ev eb) = do
  (xf', b) <- uniqueVariable xf
  let bsf = b `bind` bs
  newVariables <- mapM uniqueVariable xas
  let (xas', bsa) = unzip newVariables
  evN <- kNormalize' (bsf `bindAll` bsa) ev
  ebN <- kNormalize' bsf eb
  return $ LetRec xf' xas' evN ebN
kNormalize' bs (S.Apply e1 e2) = do
  x1 <- uniqueTempVariable
  x2 <- uniqueTempVariable
  e1N <- kNormalize' bs e1
  e2N <- kNormalize' bs e2
  return $ LetVal x1 e1N (LetVal x2 e2N (Apply x1 x2))