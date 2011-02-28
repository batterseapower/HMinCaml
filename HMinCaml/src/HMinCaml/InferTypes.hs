-----------------------------------------------------------------------------------------
{-| Module      : HMinCaml.InferTypes
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

-- We must enable undecidable instances because MonadTI is potentially
-- conflicting with an instance of MonadTI someone else could declare
{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}

module HMinCaml.InferTypes(Syntax(..), Program(..), inferTypes) where

import Data.List(union)
import Control.Monad.Error(ErrorT, runErrorT)
import Control.Monad.Trans(MonadTrans, lift)
import Control.Monad.State(StateT, MonadState, evalStateT, get, modify)

import HMinCaml.Common
import qualified HMinCaml.KNormalize as K

-- Decorate the incoming syntax tree with the inferred type signature
inferTypes :: K.Syntax -> CompilerError Program
inferTypes = evalTIError . inferAndApply
  where 
    inferAndApply :: K.Syntax -> TIError Program
    inferAndApply e = do 
      (e', t) <- inferTypes' e
      e'' <- applyCurrentSubstitution e'
      t' <- applyCurrentSubstitution t
      return $ Program { returnType = t', body = e'' }
    
    evalTIError :: TIError a -> CompilerError a
    evalTIError e = ((lift . runTIError) e) >>= (either fail return)
            
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
            | LetRec TypedIdentifier [TypedIdentifier] Syntax Syntax
            | Apply TypedIdentifier TypedIdentifier
            deriving (Eq, Show)

data Program = Program { returnType :: Type, body :: Syntax }
             deriving (Eq, Show)

type Substitution = [(Identifier, Type)]
nilSubstitution = []

(|->) :: Identifier -> Type -> Substitution
u |-> t = [(u, t)]

-- Applies the first substitution to the second, to create one which has the effect of both
(@@) ::  Substitution -> Substitution -> Substitution
s1 @@ s2 = [(x, apply s1 t) | (x, t) <- s2] ++ s1 -- We must add s1 on the end even after applying it to s2 
                                                  -- because it could contain variable mappings not present in s2

-- Fills any variable types in its type argument with those from the substitution, if possible
class Applyable t where
  apply :: Substitution -> t -> t

instance (Applyable a) => Applyable [a] where
  apply s as = map (apply s) as

instance Applyable TypedIdentifier where
  apply s (TypedIdentifier x t) = TypedIdentifier x (apply s t)

instance Applyable Type where
  apply s (TupleType ts) = TupleType (map (apply s) ts)
  apply s (FunctionType t1 t2) = FunctionType (apply s t1) (apply s t2)
  apply s t@(VariableType x) = case lookup x s of
                               Just t' -> t'
                               Nothing -> t
  apply _ t = t

instance Applyable Syntax where
  apply s (Tuple xs) = Tuple (apply s xs)
  apply s (Variable x) = Variable (apply s x)
  apply s (INegate x) = INegate (apply s x)
  apply s (IBinaryExpression op x1 x2) = IBinaryExpression op (apply s x1) (apply s x2)
  apply s (FNegate x) = FNegate (apply s x)
  apply s (FBinaryExpression op x1 x2) = FBinaryExpression op (apply s x1) (apply s x2)
  apply s (BNot x) = BNot (apply s x)
  apply s (BEqual x1 x2) = BEqual (apply s x1) (apply s x2)
  apply s (BLessEqual x1 x2) = BLessEqual (apply s x1) (apply s x2)
  apply s (If xt e1 e2) = If (apply s xt) (apply s e1) (apply s e2)
  apply s (LetVal x ev eb) = LetVal (apply s x) (apply s ev) (apply s eb)
  apply s (LetTuple xs xv eb) = LetTuple (apply s xs) (apply s xv) (apply s eb)
  apply s (LetRec x xs ev eb) = LetRec (apply s x) (apply s xs) (apply s ev) (apply s eb)
  apply s (Apply x1 x2) = Apply (apply s x1) (apply s x2)
  apply _ t = t

-- Finds all the type variables in a type expression: used for occurs checking
typeVariables :: Type -> [Identifier]
typeVariables (TupleType ts) = foldr (union . typeVariables) [] ts
typeVariables (FunctionType t1 t2) = (typeVariables t1) `union` (typeVariables t2)
typeVariables (VariableType x) = [x]
typeVariables _                = []


data Assumption = Identifier :>: Type
type Assumptions = [Assumption]
nilAssumptions = []


type TIState = (Substitution, Assumptions)
newtype TI a = TI (StateT TIState Compiler a)
             deriving (Monad, MonadState TIState, MonadCompiler)
                               
runTI :: TI a -> Compiler a
runTI (TI c) = evalStateT c (nilSubstitution, nilAssumptions)

class (Monad m) => MonadTI m where
  applyCurrentSubstitution :: (Applyable a) => a -> m a
  updateCurrentSubstitution :: Substitution -> m ()
  newTypeVariable :: m Type
  findInAssumptions :: Identifier -> m Type

instance MonadTI TI where
  applyCurrentSubstitution a = 
    let currentSubstitution = get >>= (return . fst)
    in do { s <- currentSubstitution; return (apply s a) }
    
  updateCurrentSubstitution s' = modify (\(s, as) -> (s' @@ s, as)) 
  
  newTypeVariable = do
    x <- uniqueIdentifier
    return $ VariableType x
  
  findInAssumptions x = 
    let currentAssumptions = get >>= (return . snd)
        addToAssumptions x t = modify (\(s, as) -> (s, (x :>: t) : as))
    in do
      as <- currentAssumptions
      case [t | (y :>: t) <- as, x == y] of
        []    -> do { t <- newTypeVariable; addToAssumptions x t; return t }
        (t:_) -> return t
  
instance (MonadTrans mt, MonadTI m, Monad (mt m)) => MonadTI (mt m) where
  applyCurrentSubstitution a = lift (applyCurrentSubstitution a)
  updateCurrentSubstitution s' = lift (updateCurrentSubstitution s') 
  newTypeVariable = lift newTypeVariable
  findInAssumptions x = lift (findInAssumptions x)


type TIError a = ErrorT String TI a

runTIError :: TIError a -> Compiler (Either String a)
runTIError = runTI . runErrorT

-- Unify the two types if possible and update the current substitution appropriately
unify :: Type -> Type -> TIError ()
unify t1 t2 =
  do
    t1' <- applyCurrentSubstitution t1
    t2' <- applyCurrentSubstitution t2
    unify' t1' t2'
  where
    unify' :: Type -> Type -> TIError ()
    unify' (TupleType ts1) (TupleType ts2) 
      | length ts1 == length ts2 = mapM_ (uncurry unify) (zip ts1 ts2)
      | otherwise                = fail "Cannot unify tuples of different sizes"
    unify' (FunctionType t1a t1r) (FunctionType t2a t2r) = 
      do
        unify t1a t2a
        unify t1r t2r
    unify' (VariableType x) t = bindVariable x t
    unify' t (VariableType x) = bindVariable x t
    unify' t1 t2 | t1 == t2  = return ()
                 | otherwise = fail $ "No unifier exists between " ++ show t1 ++ " and " ++ show t2

    bindVariable :: Identifier -> Type -> TIError ()
    bindVariable x t | t == VariableType x      = return ()
                     | x `elem` typeVariables t = fail "Occurs check failure"
                     | otherwise                = updateCurrentSubstitution (x |-> t)


unaryOperator :: (TypedIdentifier -> Syntax) -> Identifier -> Type -> (TIError (Syntax, Type))
unaryOperator operator x expectedType =
  do
    tx <- findInAssumptions x
    unify tx expectedType
    return (operator (TypedIdentifier x tx), tx)

binaryOperator :: (TypedIdentifier -> TypedIdentifier -> Syntax) -> 
                  Identifier -> Identifier -> 
                  Type -> Type -> (TIError (Syntax, Type))
binaryOperator operator x1 x2 inputType outputType = 
  do
    tx1 <- findInAssumptions x1
    tx2 <- findInAssumptions x2
    unify tx1 inputType
    unify tx2 inputType
    return (operator (TypedIdentifier x1 tx1) (TypedIdentifier x2 tx2), outputType)


inferTypes' :: K.Syntax -> (TIError (Syntax, Type))
inferTypes' K.Unit = return (Unit, UnitType)
inferTypes' (K.IConstant c) = return (IConstant c, IntegerType)
inferTypes' (K.FConstant c) = return (FConstant c, FloatingPointType)
inferTypes' (K.BConstant c) = return (BConstant c, BooleanType)
inferTypes' (K.Tuple xs) =
  do
    txs <- mapM findInAssumptions xs
    return $ (Tuple (zipWith TypedIdentifier xs txs), TupleType txs)
inferTypes' (K.Variable x) =
  do
    tx <- findInAssumptions x
    return $ (Variable (TypedIdentifier x tx), tx)
    
inferTypes' (K.INegate x) = unaryOperator INegate x IntegerType
inferTypes' (K.IBinaryExpression op x1 x2) = binaryOperator (IBinaryExpression op) x1 x2 IntegerType IntegerType

inferTypes' (K.FNegate x) = unaryOperator FNegate x FloatingPointType
inferTypes' (K.FBinaryExpression op x1 x2) = binaryOperator (FBinaryExpression op) x1 x2 FloatingPointType FloatingPointType

inferTypes' (K.BNot x) = unaryOperator BNot x BooleanType
inferTypes' (K.BEqual x1 x2) = binaryOperator BEqual x1 x2 IntegerType BooleanType
inferTypes' (K.BLessEqual x1 x2) = binaryOperator BLessEqual x1 x2 IntegerType BooleanType

inferTypes' (K.If xt e1 e2) =
  do
    tt <- findInAssumptions xt
    (te1, t1) <- inferTypes' e1
    (te2, t2) <- inferTypes' e2
    unify tt BooleanType
    unify t1 t2
    return (If (TypedIdentifier xt tt) te1 te2, t1)
inferTypes' (K.LetVal x ev eb) =
  do
    tx <- findInAssumptions x
    (tev, tv) <- inferTypes' ev
    unify tx tv
    (teb, tb) <- inferTypes' eb
    return (LetVal (TypedIdentifier x tv) tev teb, tb)
inferTypes' (K.LetTuple xs xv eb) =
  do
    txs <- mapM findInAssumptions xs
    txv <- findInAssumptions xv
    unify (TupleType txs) txv
    (teb, tb) <- inferTypes' eb
    return (LetTuple (zipWith TypedIdentifier xs txs) (TypedIdentifier xv txv) teb, tb)
inferTypes' (K.LetRec xf xas ev eb) =
  do
    txf <- findInAssumptions xf
    txas <- mapM findInAssumptions xas
    (etv, tev) <- inferTypes' ev
    let tbf = foldr FunctionType tev txas
    unify txf tbf
    (etb, teb) <- inferTypes' eb
    return (LetRec (TypedIdentifier xf txf) (zipWith TypedIdentifier xas txas) etv etb, teb)
inferTypes' (K.Apply xf xa) =
  do
    txf <- findInAssumptions xf
    txa <- findInAssumptions xa
    txfr <- newTypeVariable
    unify (FunctionType txa txfr) txf
    return (Apply (TypedIdentifier xf txf) (TypedIdentifier xa txa), txfr)