-----------------------------------------------------------------------------------------
{-| Module      : Tests.VariableLowering
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.VariableLowering where

import Test.HUnit

import HMinCaml.Common
import HMinCaml.VariableLowering
import qualified HMinCaml.ClosureConversion as C

assertLowersTo :: String -> C.Program -> Program -> Assertion
assertLowersTo message convertedSyntax loweredSyntax = assertEqual message (Right loweredSyntax) (runCompilerError $ lowerVariables convertedSyntax)


lowerings = TestCase $ do
  let fun1Type = (FunctionType BooleanType IntegerType)
      fun1ID = (TypedIdentifier "fun1" fun1Type)
  assertLowersTo "All-encompassing lowering test case"
    (C.Program
      [
         (fun1ID,
           (C.Function {
             C.closure = [TypedIdentifier "local1" IntegerType, TypedIdentifier "local2" IntegerType],
             C.argument = TypedIdentifier "arg1" BooleanType,
             C.body = 
               C.FunctionBody { C.returnType = IntegerType, C.expression =
                 C.If (TypedIdentifier "arg1" BooleanType) 
                 (C.Variable (TypedIdentifier "local2" IntegerType))
                 (C.Variable (TypedIdentifier "local1" IntegerType)) } }))
      ]
      C.FunctionBody { C.returnType = IntegerType, C.expression =
        (C.LetVal (TypedIdentifier "local1" IntegerType) (C.IConstant 1)
        (C.LetVal (TypedIdentifier "local2" IntegerType) (C.IConstant 2)
        (C.LetNewClosure (TypedIdentifier "local3" fun1Type) 
          (C.Closure { C.function = fun1ID, C.closeVariables = [TypedIdentifier "local1" IntegerType, TypedIdentifier "local2" IntegerType] })
        (C.LetVal (TypedIdentifier "local4" BooleanType) (C.BConstant True)
        (C.ApplyClosure (TypedIdentifier "local3" fun1Type) (TypedIdentifier "local4" BooleanType)))))) }
    )
    (Program
      [
         ("fun1",
           (Function {
             closure = [IntegerType, IntegerType],
             locals = [],
             argument = BooleanType,
             returnType = IntegerType,
             expression = If (ArgumentVariable)
                          (Variable (ClosureVariable 1))
                          (Variable (ClosureVariable 0)) }) )
      ]
      (Function {
        closure = [],
        locals = [IntegerType, IntegerType, fun1Type, BooleanType],
        argument = UnitType,
        returnType = IntegerType,
        expression = (LetVal (LocalVariable 0) (IConstant 1)
                     (LetVal (LocalVariable 1) (IConstant 2)
                     (LetNewClosure (LocalVariable 2) (Closure { function = "fun1", closeVariables = [LocalVariable 0, LocalVariable 1] })
                     (LetVal (LocalVariable 3) (BConstant True)
                     (ApplyClosure ((FunctionType BooleanType IntegerType), LocalVariable 2) (LocalVariable 3)))))) })
    )
  
tests = TestList
  [
    "All Lowerings" ~: lowerings
  ]

runTests = runTestTT tests