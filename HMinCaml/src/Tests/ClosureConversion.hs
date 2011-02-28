-----------------------------------------------------------------------------------------
{-| Module      : Tests.ClosureConversion
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.ClosureConversion where

import Test.HUnit
import Tests.Common

import HMinCaml.Common
import HMinCaml.ClosureConversion
import qualified HMinCaml.InferTypes as I


assertConvertsTo :: String -> I.Program -> Program -> Assertion
assertConvertsTo message inferredSyntax convertedSyntax = assertEqual message (Right convertedSyntax) (runCompilerError $ closureConvert inferredSyntax)


simpleConversion = TestCase $ do
  let funType = FunctionType IntegerType BooleanType
      funID = TypedIdentifier "fun" funType
  assertConvertsTo "Simple direct application with parameter unused"
    (I.Program { I.body =
      (I.LetRec funID [integerID "i"] (I.BConstant True) 
      (I.LetVal (integerID "0") (I.IConstant 1) 
      (I.Apply funID (integerID "0")))),
      I.returnType = BooleanType })
    
    (Program
      ([
        (TypedIdentifier "1" funType, 
        Function { 
          argument = (integerID "i"),
          body = FunctionBody { returnType = BooleanType, expression = BConstant True }, 
          closure = [] })
      ])
      FunctionBody { returnType = BooleanType, expression =
        (LetNewClosure funID (Closure { function = TypedIdentifier "1" funType, closeVariables = [] })
        (LetVal (integerID "0") (IConstant 1) 
        (ApplyClosure funID (integerID "0")))) })
  
  
  let funType = FunctionType FloatingPointType FloatingPointType
      funID = TypedIdentifier "fun" funType
  assertConvertsTo "Simple direct application with used parameter"
    (I.Program { I.body =
      (I.LetRec funID [floatID "param"] (I.Variable (floatID "param")) 
      (I.LetVal (floatID "0") (I.FConstant 1.1) 
      (I.Apply funID (floatID "0")))),
      I.returnType = FloatingPointType })
    
    (Program
      ([
        (TypedIdentifier "1" funType, 
        Function { 
          argument = (floatID "param"),
          body = FunctionBody { returnType = FloatingPointType, expression = (Variable (floatID "param")) }, 
          closure = [] })
      ])
      FunctionBody { returnType = FloatingPointType, expression =
        (LetNewClosure funID (Closure { function = TypedIdentifier "1" funType, closeVariables = [] })
        (LetVal (floatID "0") (FConstant 1.1) 
        (ApplyClosure funID (floatID "0")))) })
      
      
partialApplicationConversion = TestCase $ do
  let funType' = FunctionType FloatingPointType UnitType
      funType = FunctionType FloatingPointType funType'
      funID = TypedIdentifier "fun" funType
  assertConvertsTo "Partial application of two-argument function not using variables"
    (I.Program { I.body =
      (I.LetRec funID [floatID "param1", floatID "param2"] (I.Unit) 
      (I.LetVal (floatID "i0") (I.FConstant 1.1) 
      (I.LetVal (TypedIdentifier "o0" funType') (I.Apply funID (floatID "i0"))
      I.Unit))),
      I.returnType = UnitType })
    
    (Program
      ([
        (TypedIdentifier "3" funType, 
        Function { 
          argument = (floatID "param1"),
          body = FunctionBody { returnType = funType', expression = 
            (LetNewClosure (TypedIdentifier "2" funType') (Closure { function = TypedIdentifier "1" funType', closeVariables = [floatID "param1"] })
            (Variable (TypedIdentifier "2" funType'))) }, 
          closure = [] })
        ,
        (TypedIdentifier "1" funType', 
        Function { 
          argument = (floatID "param2"),
          body = FunctionBody { returnType = UnitType, expression = Unit }, 
          closure = [(floatID "param1")] })
      ])
      FunctionBody { returnType = UnitType, expression = 
        (LetNewClosure funID (Closure { function = TypedIdentifier "3" funType, closeVariables = [] })
        (LetVal (floatID "i0") (FConstant 1.1) 
        (LetVal (TypedIdentifier "o0" funType') (ApplyClosure funID (floatID "i0"))
        Unit))) })
      
  assertConvertsTo "Partial application of two-argument function using variables"
    (I.Program { I.body =
      (I.LetRec funID [floatID "param1", floatID "param2"] (I.FBinaryExpression Add (floatID "param1") (floatID "param2")) 
      (I.LetVal (floatID "i0") (I.FConstant 1.1) 
      (I.LetVal (TypedIdentifier "o0" funType') (I.Apply funID (floatID "i0"))
      I.Unit))),
      I.returnType = UnitType })
    
    (Program
      ([
        (TypedIdentifier "3" funType, 
        Function { 
          argument = (floatID "param1"),
          body = FunctionBody { returnType = funType', expression =
            (LetNewClosure (TypedIdentifier "2" funType') (Closure { function = TypedIdentifier "1" funType', closeVariables = [floatID "param1"] })
            (Variable (TypedIdentifier "2" funType'))) }, 
          closure = [] })
        ,
        (TypedIdentifier "1" funType', 
        Function { 
          argument = (floatID "param2"),
          body = FunctionBody { returnType = UnitType, expression = (FBinaryExpression Add (floatID "param1") (floatID "param2")) }, 
          closure = [floatID "param1"] })
      ])
      FunctionBody { returnType = UnitType, expression =
        (LetNewClosure funID (Closure { function = TypedIdentifier "3" funType, closeVariables = [] })
        (LetVal (floatID "i0") (FConstant 1.1) 
        (LetVal (TypedIdentifier "o0" funType') (ApplyClosure funID (floatID "i0"))
        Unit))) })
      
  assertConvertsTo "Full application of two-argument function using variables"
    (I.Program { I.body =
      (I.LetRec funID [floatID "param1", floatID "param2"] (I.FBinaryExpression Add (floatID "param1") (floatID "param2")) 
      (I.LetVal (floatID "i0") (I.FConstant 1.1) 
      (I.LetVal (TypedIdentifier "o0" funType') (I.Apply funID (floatID "i0"))
      (I.LetVal (TypedIdentifier "o1" FloatingPointType) (I.Apply (TypedIdentifier "o0" funType') (floatID "i0"))
      I.Unit)))),
      I.returnType = UnitType })
    
    (Program
      ([
        (TypedIdentifier "3" funType, 
        Function { 
          argument = (floatID "param1"),
          body = FunctionBody { returnType = funType', expression =
            (LetNewClosure (TypedIdentifier "2" funType') (Closure { function = TypedIdentifier "1" funType', closeVariables = [floatID "param1"] })
            (Variable (TypedIdentifier "2" funType'))) }, 
          closure = [] })
        ,
        (TypedIdentifier "1" funType', 
        Function { 
          argument = (floatID "param2"),
          body = FunctionBody { returnType = UnitType, expression = FBinaryExpression Add (floatID "param1") (floatID "param2") }, 
          closure = [floatID "param1"] })
      ])
      FunctionBody { returnType = UnitType, expression =
        (LetNewClosure funID (Closure { function = TypedIdentifier "3" funType, closeVariables = [] })
        (LetVal (floatID "i0") (FConstant 1.1) 
        (LetVal (TypedIdentifier "o0" funType') (ApplyClosure funID (floatID "i0"))
        (LetVal (TypedIdentifier "o1" FloatingPointType) (ApplyClosure (TypedIdentifier "o0" funType') (floatID "i0"))
        Unit)))) })
        
      
closingEnvironment = TestCase $ do 
  let funType = FunctionType BooleanType BooleanType
      funID = TypedIdentifier "fun" funType
      closedID = TypedIdentifier "boolvar" BooleanType
  assertConvertsTo "Simple direct application with closing and used parameter"
    (I.Program { I.body =
      (I.LetVal closedID (I.BConstant True)
      (I.LetRec funID [floatID "param"] (I.If closedID (I.Variable (floatID "param")) (I.FConstant 1.0))
      (I.LetVal (floatID "0") (I.FConstant 1.1) 
      (I.Apply funID (floatID "0"))))),
      I.returnType = BooleanType })
    
    (Program
      ([
        (TypedIdentifier "1" funType, 
        Function { 
          argument = (floatID "param"),
          body = FunctionBody { returnType = BooleanType, expression = If closedID (Variable (floatID "param")) (FConstant 1.0) }, 
          closure = [closedID] })
      ])
      FunctionBody { returnType = BooleanType, expression = 
        (LetVal closedID (BConstant True)
        (LetNewClosure funID (Closure { function = TypedIdentifier "1" funType, closeVariables = [closedID] })
        (LetVal (floatID "0") (FConstant 1.1) 
        (ApplyClosure funID (floatID "0"))))) })
    
    
  let funType' = FunctionType FloatingPointType UnitType
      funType = FunctionType FloatingPointType funType'
      funID = TypedIdentifier "fun" funType
      closedID = TypedIdentifier "boolvar" FloatingPointType
  assertConvertsTo "Partial application of two-argument function with closing and one used parameter"
    (I.Program { I.body =
      (I.LetVal closedID (I.FConstant 1.0)
      (I.LetRec funID [floatID "param1", floatID "param2"] (I.FBinaryExpression Add closedID (floatID "param2")) 
      (I.LetVal (floatID "i0") (I.FConstant 1.1) 
      (I.LetVal (TypedIdentifier "o0" funType') (I.Apply funID (floatID "i0"))
      I.Unit)))),
      I.returnType = UnitType })
    
    (Program
      ([
        (TypedIdentifier "3" funType, 
        Function { 
          argument = (floatID "param1"),
          body = FunctionBody { returnType = funType', expression =
            (LetNewClosure (TypedIdentifier "2" funType') (Closure { function = TypedIdentifier "1" funType', closeVariables = [floatID "param1", closedID] })
            (Variable (TypedIdentifier "2" funType'))) }, 
          closure = [closedID] })
        ,
        (TypedIdentifier "1" funType', 
        Function { 
          argument = (floatID "param2"),
          body = FunctionBody { returnType = UnitType, expression = FBinaryExpression Add closedID (floatID "param2") }, 
          closure = [floatID "param1", closedID] })
      ])
      FunctionBody { returnType = UnitType, expression =
        (LetVal closedID (FConstant 1.0)
        (LetNewClosure funID (Closure { function = TypedIdentifier "3" funType, closeVariables = [closedID] })
        (LetVal (floatID "i0") (FConstant 1.1) 
        (LetVal (TypedIdentifier "o0" funType') (ApplyClosure funID (floatID "i0"))
        Unit)))) })
    
  let funType = FunctionType IntegerType IntegerType
      funID = TypedIdentifier "fun" funType
  assertConvertsTo "Recursive function application"
    (I.Program { I.body =
      (I.LetRec funID [integerID "param1"] (I.Apply funID (integerID "param1")) 
      (I.LetVal (integerID "i0") (I.IConstant 1) 
      (I.LetVal (integerID "o0") (I.Apply funID (integerID "i0"))
      I.Unit))),
      I.returnType = UnitType })
    
    (Program
      ([
        (TypedIdentifier "1" funType, 
        Function { 
          argument = (integerID "param1"),
          body = FunctionBody { returnType = IntegerType, expression = 
            (LetNewClosure funID (Closure { function = TypedIdentifier "1" funType, closeVariables = [] })
            (ApplyClosure funID (integerID "param1"))) }, 
          closure = [] })
      ])
      FunctionBody { returnType = UnitType, expression =
        (LetNewClosure funID (Closure { function = TypedIdentifier "1" funType, closeVariables = [] })
        (LetVal (integerID "i0") (IConstant 1) 
        (LetVal (integerID "o0") (ApplyClosure funID (integerID "i0"))
        Unit))) })
    
  
tests = TestList
  [
    "Simple Closure Conversion" ~: simpleConversion,
    "Conversion Of Partial Application" ~: partialApplicationConversion,
    "Closing Environment Variables" ~: closingEnvironment
  ]

runTests = runTestTT tests