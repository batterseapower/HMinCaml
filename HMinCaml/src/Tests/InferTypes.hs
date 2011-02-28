-----------------------------------------------------------------------------------------
{-| Module      : Tests.InferTypes
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.InferTypes where

import Test.HUnit
import Tests.Common
import Control.Monad(when)

import HMinCaml.Common
import HMinCaml.InferTypes
import qualified HMinCaml.KNormalize as K

assertTypeChecksTo :: String -> K.Syntax -> Program -> Assertion
assertTypeChecksTo message parseSyntax inferredSyntax = assertEqual message (Right inferredSyntax) (runCompilerError $ inferTypes parseSyntax)

assertTypeCheckFails :: String -> K.Syntax -> Assertion
assertTypeCheckFails message parseSyntax = when (fails (runCompilerError $ inferTypes parseSyntax)) (assertFailure $ message ++ ": parse did not fail!")
  where
    fails = either (const False) (const True)
                                               
                                                
occursCheck = TestCase $ do
  assertTypeCheckFails "Occurs check not performed" $ K.LetRec "xf" ["x"] (K.LetVal "0" (K.Variable "x") (K.LetVal "1" (K.Variable "x") (K.Apply "0" "1"))) (K.LetVal "2" (K.IConstant 2) (K.Apply "xf" "2"))
  
constants = TestCase $ do
  assertTypeChecksTo "Simple unit constant" (K.LetVal "x" K.Unit (K.BConstant True))
                                            (Program { body = (LetVal (unitID "x") Unit (BConstant True)), returnType = BooleanType })
  assertTypeChecksTo "Simple integer constant" (K.LetVal "x" (K.IConstant 1) (K.BConstant True))
                                               (Program { body = (LetVal (integerID "x") (IConstant 1) (BConstant True)), returnType = BooleanType })
  assertTypeChecksTo "Simple float constant" (K.LetVal "x" (K.FConstant 1337.0) (K.BConstant False))
                                             (Program { body = (LetVal (floatID "x") (FConstant 1337.0) (BConstant False)), returnType = BooleanType })
  assertTypeChecksTo "Simple boolean constant" (K.LetVal "x" (K.BConstant True) K.Unit)
                                               (Program { body = (LetVal (booleanID "x") (BConstant True) Unit), returnType = UnitType })

tuples = TestCase $ do
  assertTypeChecksTo "Simple tuple" 
    (K.LetVal "x" 
      (K.LetVal "0" (K.IConstant 13) 
       (K.LetVal "1" (K.BConstant True) 
       (K.Tuple ["0", "1"])))
    (K.FConstant (-1.0)))
    (Program { body = 
      (LetVal (TypedIdentifier "x" (TupleType [IntegerType, BooleanType])) 
        (LetVal (integerID "0") (IConstant 13) 
        (LetVal (booleanID "1") (BConstant True) 
        (Tuple [integerID "0", booleanID "1"])))
      (FConstant (-1.0))),
      returnType = FloatingPointType
    })
  
  assertTypeChecksTo "Nested tuple" 
    (K.LetVal "x" 
      (K.LetVal "0" (K.FConstant 15.0) 
      (K.LetVal "1" (K.Unit)
      (K.LetVal "2" 
        (K.LetVal "3" (K.IConstant 1)
        (K.LetVal "4" (K.IConstant 1) 
        (K.Tuple ["3", "4"])))
      (K.Tuple ["0", "1", "2"]))))
    K.Unit)
    (Program { body = 
      (LetVal (TypedIdentifier "x" (TupleType [FloatingPointType, UnitType, TupleType [IntegerType, IntegerType]]))
        (LetVal (floatID "0") (FConstant 15.0) 
        (LetVal (unitID "1") (Unit)
        (LetVal (TypedIdentifier "2" (TupleType [IntegerType, IntegerType]))
          (LetVal (integerID "3") (IConstant 1)
          (LetVal (integerID "4") (IConstant 1) 
          (Tuple [(integerID "3"), (integerID "4")])))
        (Tuple [floatID "0", unitID "1", TypedIdentifier "2" (TupleType [IntegerType, IntegerType])]))))
      Unit),
      returnType = UnitType
    })
  
integerArithmetic = TestCase $ do
  assertTypeChecksTo "Simple integer operations" 
    (K.LetVal "y" 
      (K.LetVal "0" (K.IConstant 2)
      (K.LetVal "1" (K.IConstant 3) 
      (K.LetVal "3" (K.INegate "0")
      (K.IBinaryExpression Add "3" "1"))))
    K.Unit)
    (Program { body = 
      (LetVal (integerID "y")
        (LetVal (integerID "0") (IConstant 2)
        (LetVal (integerID "1") (IConstant 3) 
        (LetVal (integerID "3") (INegate (integerID "0"))
        (IBinaryExpression Add (integerID "3") (integerID "1")))))
      Unit),
      returnType = UnitType
    })
    
  assertTypeChecksTo "Nested integer operations" 
    (K.LetVal "y" 
      (K.LetVal "0" (K.IConstant 14)
      (K.LetVal "1" (K.INegate "0")
      (K.LetVal "2" (K.IConstant 2)
      (K.LetVal "3" (K.IBinaryExpression Subtract "1" "2")
      (K.LetVal "4" (K.INegate "3")
      (K.LetVal "5" (K.IConstant 3)
      (K.LetVal "6" (K.IConstant 4)
      (K.LetVal "7" (K.IBinaryExpression Divide "5" "6")
      (K.IBinaryExpression Add "4" "7")))))))))
    K.Unit)
    (Program { body = 
      (LetVal (integerID "y") 
        (LetVal (integerID "0") (IConstant 14)
        (LetVal (integerID "1") (INegate (integerID "0"))
        (LetVal (integerID "2") (IConstant 2)
        (LetVal (integerID "3") (IBinaryExpression Subtract (integerID "1") (integerID "2"))
        (LetVal (integerID "4") (INegate (integerID "3"))
        (LetVal (integerID "5") (IConstant 3)
        (LetVal (integerID "6") (IConstant 4)
        (LetVal (integerID "7") (IBinaryExpression Divide (integerID "5") (integerID "6"))
        (IBinaryExpression Add (integerID "4") (integerID "7"))))))))))
      Unit),
      returnType = UnitType
    })
    
  assertTypeCheckFails "Unary integer operation on float" (K.LetVal "0" (K.FConstant 2.0) (K.INegate "0"))
  assertTypeCheckFails "Binary integer operation on float" (K.LetVal "0" (K.FConstant 2.0) (K.LetVal "1" (K.IConstant 2) (K.IBinaryExpression Add "0" "1")))
  
 
floatArithmetic = TestCase $ do
  assertTypeChecksTo "Simple float operations" 
    (K.LetVal "y" 
      (K.LetVal "0" (K.FConstant 2.1)
      (K.LetVal "1" (K.FConstant 3.2) 
      (K.LetVal "3" (K.FNegate "0")
      (K.FBinaryExpression Add "3" "1"))))
    K.Unit)
    (Program { body = 
      (LetVal (floatID "y")
        (LetVal (floatID "0") (FConstant 2.1)
        (LetVal (floatID "1") (FConstant 3.2) 
        (LetVal (floatID "3") (FNegate (floatID "0"))
        (FBinaryExpression Add (floatID "3") (floatID "1")))))
      Unit),
      returnType = UnitType
    })
    
  assertTypeChecksTo "Nested float operations" 
    (K.LetVal "y" 
      (K.LetVal "0" (K.FConstant 14.3)
      (K.LetVal "1" (K.FNegate "0")
      (K.LetVal "2" (K.FConstant 2.4)
      (K.LetVal "3" (K.FBinaryExpression Subtract "1" "2")
      (K.LetVal "4" (K.FNegate "3")
      (K.LetVal "5" (K.FConstant 3.5)
      (K.LetVal "6" (K.FConstant 4.6)
      (K.LetVal "7" (K.FBinaryExpression Divide "5" "6")
      (K.FBinaryExpression Add "4" "7")))))))))
    K.Unit)
    (Program { body = 
      (LetVal (floatID "y") 
        (LetVal (floatID "0") (FConstant 14.3)
        (LetVal (floatID "1") (FNegate (floatID "0"))
        (LetVal (floatID "2") (FConstant 2.4)
        (LetVal (floatID "3") (FBinaryExpression Subtract (floatID "1") (floatID "2"))
        (LetVal (floatID "4") (FNegate (floatID "3"))
        (LetVal (floatID "5") (FConstant 3.5)
        (LetVal (floatID "6") (FConstant 4.6)
        (LetVal (floatID "7") (FBinaryExpression Divide (floatID "5") (floatID "6"))
        (FBinaryExpression Add (floatID "4") (floatID "7"))))))))))
      Unit),
      returnType = UnitType
    })
    
  assertTypeCheckFails "Unary float operation on integer" (K.LetVal "0" (K.IConstant 2) (K.FNegate "0"))
  assertTypeCheckFails "Binary float operation on integer" (K.LetVal "0" (K.FConstant 2.0) (K.LetVal "1" (K.IConstant 2) (K.FBinaryExpression Add "0" "1")))
  
  
booleanComparisons = TestCase $ do
  assertTypeChecksTo "Simple boolean operations" 
    (K.LetVal "y" 
      (K.LetVal "0" (K.IConstant 2)
      (K.LetVal "1" (K.IConstant 10)
      (K.BLessEqual "0" "1")))
    K.Unit)
    (Program { body = 
      (LetVal (booleanID "y")
        (LetVal (integerID "0") (IConstant 2)
        (LetVal (integerID "1") (IConstant 10)
        (BLessEqual (integerID "0") (integerID  "1"))))
      Unit),
      returnType = UnitType
    })
  
  assertTypeChecksTo "Nested boolean operations"
    (K.LetVal "y" 
      (K.LetVal "0" (K.IConstant 14)
      (K.LetVal "1" (K.IConstant 2)
      (K.LetVal "2" (K.BEqual "0" "1")
      (K.BNot "2"))))
    K.Unit)
    (Program { body = 
      (LetVal (booleanID "y")
        (LetVal (integerID "0") (IConstant 14)
        (LetVal (integerID "1") (IConstant 2)
        (LetVal (booleanID "2") (BEqual (integerID "0") (integerID "1"))
        (BNot (booleanID "2")))))
      Unit),
      returnType = UnitType
    })
  
  assertTypeCheckFails "Unary boolean operation on integer" (K.LetVal "0" (K.IConstant 2) (K.BNot "0"))
  assertTypeCheckFails "Binary boolean equality on boolean" (K.LetVal "0" (K.BConstant True) (K.LetVal "0" (K.IConstant 2) (K.BEqual "0" "1")))
  assertTypeCheckFails "Binary boolean less equality on boolean" (K.LetVal "0" (K.IConstant 1) (K.LetVal "1" (K.BConstant False) (K.BLessEqual "0" "1")))
  
  
ifExpressions = TestCase $ do
  assertTypeChecksTo "Simple conditional" 
    (K.LetVal "y"
      (K.LetVal "test" (K.BConstant True) 
      (K.If "test" 
        (K.IConstant 2) 
        (K.IConstant 10)))
    K.Unit)
    (Program { body = 
      (LetVal (integerID "y")
        (LetVal (booleanID "test") (BConstant True) 
        (If (booleanID "test")
          (IConstant 2) 
          (IConstant 10)))
      Unit),
      returnType = UnitType
    })
    
  assertTypeCheckFails "Non-boolean test type" (K.LetVal "0" (K.IConstant 1) (K.If "0" (K.IConstant 2) (K.IConstant 10)))
  assertTypeCheckFails "Inconsistent result types" (K.LetVal "0" (K.BConstant True) (K.If "0" (K.IConstant 2) (K.FConstant 2.0)))
  
  
simpleLetExpressions = TestCase $ do
  assertTypeChecksTo "Simple let value" 
    (K.LetVal "blah" (K.IConstant 2) K.Unit)
    (Program { body = (LetVal (integerID "blah") (IConstant 2) Unit), returnType = UnitType })
  
  assertTypeChecksTo "Let value type propagation" 
    (K.LetVal "x" 
      (K.LetVal "y" (K.BConstant True) (K.Variable "y")) 
    K.Unit)
    (Program { body = 
      (LetVal (booleanID "x")
        (LetVal (TypedIdentifier "y" BooleanType) (BConstant True) (Variable (booleanID "y"))) 
      Unit),
      returnType = UnitType
    })
  
  assertTypeChecksTo "Simple let tuple value"
    (K.LetVal "0" (K.IConstant 1)
    (K.LetVal "1" (K.BConstant False)
    (K.LetVal "3" (K.Tuple ["0", "1"])
    (K.LetTuple ["x", "y"] "3" K.Unit))))
    (Program { body = 
      (LetVal (integerID "0") (IConstant 1)
      (LetVal (booleanID "1") (BConstant False)
      (LetVal (TypedIdentifier "3" (TupleType [IntegerType, BooleanType])) (Tuple [integerID "0", booleanID "1"])
      (LetTuple [integerID "x", booleanID "y"] (TypedIdentifier "3" (TupleType [IntegerType, BooleanType])) Unit)))),
      returnType = UnitType
    })
  
  assertTypeCheckFails "Let tuple with non-tuple" (K.LetVal "0" (K.IConstant 1) (K.LetTuple ["x", "y"] "0" K.Unit))
  assertTypeCheckFails "Let tuple with wrong size tuple" (K.LetVal "1" (K.IConstant 1) (K.LetVal "2" (K.FConstant 0.0) (K.LetVal "0" (K.Tuple ["1", "2"]) (K.LetTuple ["x", "y", "z"] "0" K.Unit))))


recursiveLetExpressions = TestCase $ do
  assertTypeChecksTo "Simple recursive let" 
    (K.LetRec "foo" ["x"] (K.IConstant 2) (K.Variable "foo"))
    (Program { body = 
      (LetRec (TypedIdentifier "foo" (FunctionType (VariableType "2") IntegerType)) [TypedIdentifier "x" (VariableType "2")] (IConstant 2) (Variable (TypedIdentifier "foo" (FunctionType (VariableType "2") IntegerType)))),
      returnType = (FunctionType (VariableType "2") IntegerType)
    })
  
  let barType = FunctionType IntegerType BooleanType
  assertTypeChecksTo "Function application"
    (K.LetRec "bar" ["y"] 
      (K.BConstant True) 
      (K.LetVal "x" 
        (K.LetVal "0" (K.Variable "bar") 
        (K.LetVal "1" (K.IConstant 1) 
        (K.Apply "0" "1"))) 
      K.Unit))
    (Program { body = 
      (LetRec (TypedIdentifier "bar" barType) [integerID "y"] 
        (BConstant True) 
        (LetVal (booleanID "x")
          (LetVal (TypedIdentifier "0" barType) (Variable (TypedIdentifier "bar" barType)) 
          (LetVal (integerID "1") (IConstant 1) 
          (Apply (TypedIdentifier "0" barType) (integerID "1")))) 
        Unit)),
      returnType = UnitType
    })
      

tests = TestList
  [
    "Occurs Check" ~: occursCheck,
    "Constants" ~: constants,
    "Tuples" ~: tuples,
    "Integer Arithmetic" ~: integerArithmetic,
    "Float Arithmetic" ~: floatArithmetic,
    "Boolean Comparisions" ~: booleanComparisons,
    "If Expressions" ~: ifExpressions,
    "Simple Let Expressions" ~: simpleLetExpressions,
    "Recursive Let Expressions" ~: recursiveLetExpressions
  ]

runTests = runTestTT tests