-----------------------------------------------------------------------------------------
{-| Module      : Tests.Parser
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.Parser where

import Test.HUnit
import Control.Monad(when)

import HMinCaml.Common
import HMinCaml.Parser

assertParsesTo :: String -> String -> Syntax -> Assertion
assertParsesTo message text expected = case runCompilerError $ parse text of 
                                       Left x -> fail $ message ++ ":\r\n" ++ show x
                                       Right actual -> assertEqual message expected actual

assertParseFails :: String -> String -> Assertion
assertParseFails message text = when (fails (runCompilerError $ parse text)) (assertFailure $ message ++ ": parse did not fail!")
  where
    fails = either (const False) (const True)

integerConstants = TestCase $ do
  {
    assertParsesTo "Zero constant" "0" $ IConstant 0;
    assertParsesTo "Single digit constant" "1" $ IConstant 1;
    assertParsesTo "Multi digit constant" "10" $ IConstant 10;
    assertParsesTo "Massive constant" "987654321987654321987654321" $ IConstant 987654321987654321987654321;
  }
                                
realConstants = TestCase $ do
  {
    assertParsesTo "Zero constant" "0.0" $ FConstant 0.0;
    assertParsesTo "Single digit constant" "1.0" $ FConstant 1.0;
    assertParsesTo "Single digit constant with floating point digits" "0.12" $ FConstant 0.12;
    assertParsesTo "Multi digit constant" "10.0" $ FConstant 10.0;
    assertParsesTo "Multi digit constant with floating point digits" "1234.56789" $ FConstant 1234.56789
  }

booleanConstants = TestCase $ do
  {
    assertParsesTo "True" "true" $ BConstant True;
    assertParsesTo "False" "false" $ BConstant False;
    assertParsesTo "Invalid True" "True" $ Variable "True";
    assertParsesTo "Invalid False" "False" $ Variable "False"
  }

integerArithmetic = TestCase $ do
  {
    assertParsesTo "Negation" "-1" $ INegate (IConstant 1);
    assertParsesTo "IBinaryExpression Add Associativitity" "1+2+3+4" $ IBinaryExpression Add (IBinaryExpression Add (IBinaryExpression Add (IConstant 1) (IConstant 2)) (IConstant 3)) (IConstant 4);
    assertParsesTo "IBinaryExpression Subtract Associativitity" "1-2-3-4" $ IBinaryExpression Subtract (IBinaryExpression Subtract (IBinaryExpression Subtract (IConstant 1) (IConstant 2)) (IConstant 3)) (IConstant 4);
    assertParsesTo "Division Associativitity" "1/2/3/4" $ IBinaryExpression Divide (IBinaryExpression Divide (IBinaryExpression Divide (IConstant 1) (IConstant 2)) (IConstant 3)) (IConstant 4);
    assertParsesTo "Operator precedence 1" "1-2+3" $ IBinaryExpression Subtract (IConstant 1) (IBinaryExpression Add (IConstant 2) (IConstant 3));
    assertParsesTo "Operator precedence 2" "1/2*3" $ IBinaryExpression Multiply (IBinaryExpression Divide (IConstant 1) (IConstant 2)) (IConstant 3);
    assertParsesTo "Operator precedence 3" "1/2*-3+4-5" $ IBinaryExpression Subtract (IBinaryExpression Add (IBinaryExpression Multiply (IBinaryExpression Divide (IConstant 1) (IConstant 2)) (INegate(IConstant 3))) (IConstant 4)) (IConstant 5);
    assertParsesTo "Bracketing 1" "(1)" $ IConstant 1;
    assertParsesTo "Bracketing 2" "(1+2)" $ IBinaryExpression Add (IConstant 1) (IConstant 2);
    assertParsesTo "Bracketing overrides precedence" "1/(2*(-(3)+(4-5)))" $ IBinaryExpression Divide (IConstant 1) (IBinaryExpression Multiply (IConstant 2) (IBinaryExpression Add (INegate (IConstant 3)) (IBinaryExpression Subtract (IConstant 4) (IConstant 5))));
    assertParsesTo "Tricky operator precedence 1" "-1-2" $ IBinaryExpression Subtract (INegate (IConstant 1)) (IConstant 2);
    assertParsesTo "Tricky operator precedence 2" "-1-(-2)" $ IBinaryExpression Subtract (INegate (IConstant 1)) (INegate (IConstant 2));
    assertParsesTo "Tricky operator precedence 3" "-1--2" $ IBinaryExpression Subtract (INegate (IConstant 1)) (INegate (IConstant 2));
  }
  
realArithmetic = TestCase $ do
  {
    assertParsesTo "Negation" "-.1.0" $ FNegate (FConstant 1.0);
    assertParsesTo "Operator precedence" "1.0/.2.0*.-.3.0+.4.0-.5.0" $ FBinaryExpression Subtract (FBinaryExpression Add (FBinaryExpression Multiply (FBinaryExpression Divide (FConstant 1.0) (FConstant 2.0)) (FNegate (FConstant 3.0))) (FConstant 4.0)) (FConstant 5.0);
    assertParsesTo "Bracketing 1" "(1.0)" $ FConstant 1;
    assertParsesTo "Bracketing 2" "(1.0+.2.0)" $ FBinaryExpression Add (FConstant 1) (FConstant 2);
    assertParsesTo "Bracketing overrides precedence" "1.0/.(2.0*.(-.(3.0)+.(4.0-.5.0)))" $ FBinaryExpression Divide (FConstant 1) (FBinaryExpression Multiply (FConstant 2) (FBinaryExpression Add (FNegate (FConstant 3)) (FBinaryExpression Subtract (FConstant 4) (FConstant 5))));
    assertParsesTo "Tricky operator precedence 1" "-.1.0-.2.0" $ FBinaryExpression Subtract (FNegate (FConstant 1)) (FConstant 2);
    assertParsesTo "Tricky operator precedence 2" "-.1.0-.(-.2.0)" $ FBinaryExpression Subtract (FNegate (FConstant 1)) (FNegate (FConstant 2));
    assertParsesTo "Tricky operator precedence 3" "-.1.0-.-.2.0" $ FBinaryExpression Subtract (FNegate (FConstant 1)) (FNegate (FConstant 2));
  }

mixedArithmetic = TestCase $ do
  {
    assertParsesTo "Mixed arithmetic works" "1*1.123+.4/(1-2-.3.456)" $ FBinaryExpression Add (IBinaryExpression Multiply (IConstant 1) (FConstant 1.123)) (IBinaryExpression Divide (IConstant 4) (FBinaryExpression Subtract (IBinaryExpression Subtract (IConstant 1) (IConstant 2)) (FConstant 3.456)));
    assertParsesTo "Mixed arithmetic with variables" "1-(x*.2.0)+(y/2)" $ IBinaryExpression Subtract (IConstant 1) (IBinaryExpression Add (FBinaryExpression Multiply (Variable "x") (FConstant 2.0)) (IBinaryExpression Divide (Variable "y") (IConstant 2)))
  }
  
equality = TestCase $ do
  {
    assertParsesTo "Vanilla equality" "1=2" $ BBinaryExpression Equal (IConstant 1) (IConstant 2);
    assertParsesTo "Expression equality 1" "(1*2)=(x + 1)" $ BBinaryExpression Equal (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (Variable "x") (IConstant 1));
    assertParsesTo "Expression equality 2" "1*2=igloo + 1" $ BBinaryExpression Equal (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (Variable "igloo") (IConstant 1));
    assertParsesTo "Expression equality 3" "1*2=x + 1=true" $ BBinaryExpression Equal (BBinaryExpression Equal (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (Variable "x") (IConstant 1))) (BConstant True);
    assertParsesTo "Equality with a let" "1*2 = let loofah = 2 in 2" $ BBinaryExpression Equal (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (LetVal "loofah" (IConstant 2) (IConstant 2))
  }
  
notOperator = TestCase $ do
  {
    assertParsesTo "Vanilla not" "!true" $ BNot (BConstant True);
    assertParsesTo "BNot with arithmetic subexpression" "!1" $ BNot (IConstant 1);
    assertParsesTo "BNot with boolean subexpression" "!(true=false)" $ BNot (BBinaryExpression Equal (BConstant True) (BConstant False));
    assertParsesTo "BNot and BBinaryExpression Equal precedence" "!true=false" $ BBinaryExpression Equal (BNot (BConstant True)) (BConstant False)
  }

lessOperator = TestCase $ do
  {
    assertParsesTo "Vanilla <" "1<2" $ BBinaryExpression Less (IConstant 1) (IConstant 2);
    assertParsesTo "< with boolean subexpression" "true<2" $ BBinaryExpression Less (BConstant True) (IConstant 2);
    assertParsesTo "< with arithmetic subexpression" "(1 * 2)<(5 + 4)" $ BBinaryExpression Less (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "< and arithmetic precedence" "1*2<5+4" $ BBinaryExpression Less (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "< and equality precedence" "1<2=3<4" $ BBinaryExpression Equal (BBinaryExpression Less (IConstant 1) (IConstant 2)) (BBinaryExpression Less (IConstant 3) (IConstant 4))
  }
  
lessEqualOperator = TestCase $ do
  {
    assertParsesTo "Vanilla <=" "1<=2" $ BBinaryExpression LessEqual (IConstant 1) (IConstant 2);
    assertParsesTo "<= with boolean subexpression" "true<=2" $ BBinaryExpression LessEqual (BConstant True) (IConstant 2);
    assertParsesTo "<= with arithmetic subexpression" "(1 * 2)<=(5 + 4)" $ BBinaryExpression LessEqual (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "<= and arithmetic precedence" "1*2<=5+4" $ BBinaryExpression LessEqual (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "<= and equality precedence" "1<=2=3<=4" $ BBinaryExpression Equal (BBinaryExpression LessEqual (IConstant 1) (IConstant 2)) (BBinaryExpression LessEqual (IConstant 3) (IConstant 4))
  }
  
greaterOperator = TestCase $ do
  {
    assertParsesTo "Vanilla >" "1>2" $ BBinaryExpression Greater (IConstant 1) (IConstant 2);
    assertParsesTo "> with boolean subexpression" "true>2" $ BBinaryExpression Greater (BConstant True) (IConstant 2);
    assertParsesTo "> with arithmetic subexpression" "(1 * 2)>(5 + 4)" $ BBinaryExpression Greater (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "> and arithmetic precedence" "1*2>5+4" $ BBinaryExpression Greater (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo "> and equality precedence" "1>2=3>4" $ BBinaryExpression Equal (BBinaryExpression Greater (IConstant 1) (IConstant 2)) (BBinaryExpression Greater (IConstant 3) (IConstant 4))
  }
  
greaterEqualOperator = TestCase $ do
  {
    assertParsesTo "Vanilla >=" "1>=2" $ BBinaryExpression GreaterEqual (IConstant 1) (IConstant 2);
    assertParsesTo ">= with boolean subexpression" "true>=2" $ BBinaryExpression GreaterEqual (BConstant True) (IConstant 2);
    assertParsesTo ">= with arithmetic subexpression" "(1 * 2)>=(5 + 4)" $ BBinaryExpression GreaterEqual (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo ">= and arithmetic precedence" "1*2>=5+4" $ BBinaryExpression GreaterEqual (IBinaryExpression Multiply (IConstant 1) (IConstant 2)) (IBinaryExpression Add (IConstant 5) (IConstant 4));
    assertParsesTo ">= and equality precedence" "1>=2=3>=4" $ BBinaryExpression Equal (BBinaryExpression GreaterEqual (IConstant 1) (IConstant 2)) (BBinaryExpression GreaterEqual (IConstant 3) (IConstant 4))
  }

unit = TestCase $ do
  {
    assertParsesTo "Unit" "()" $ Unit;
  }
  
tuples = TestCase $ do
  {
    assertParsesTo "Tuple of two constants" "(1, 2)" $ Tuple [IConstant 1, IConstant 2];
    assertParsesTo "Tuple of two expressions" "(1 * true, let x = 1 + 2 in y)" $ Tuple [IBinaryExpression Multiply (IConstant 1) (BConstant True), LetVal "x" (IBinaryExpression Add (IConstant 1) (IConstant 2)) (Variable "y")];
    assertParsesTo "Tuple of many expressions" "(1, 2, 3, 4, 5, 6, 7, 8, 9)" $ Tuple (map IConstant [1..9])
  }

letValExpressions = TestCase $ do
  {
    assertParsesTo "Vanilla let expression" "let x = 1 in 2" $ LetVal "x" (IConstant 1) (IConstant 2);
    assertParsesTo "Bracketed let expression 1" "let x = (1) in 2" $ LetVal "x" (IConstant 1) (IConstant 2);
    assertParsesTo "Bracketed let expression 2" "let (x) = 1 in 2" $ LetVal "x" (IConstant 1) (IConstant 2);
    assertParsesTo "Nested let expression" "let x = let y = 1 in 2 in 3" $ LetVal "x" (LetVal "y" (IConstant 1) (IConstant 2)) (IConstant 3);
	  assertParsesTo "Let expressions and arithmetic 1" "1 + let x = 1 in 2" $ IBinaryExpression Add (IConstant 1) (LetVal "x" (IConstant 1) (IConstant 2));
	  assertParsesTo "Let expressions and arithmetic 2" "let x = 1 in 2 + 3" $ LetVal "x" (IConstant 1) (IBinaryExpression Add (IConstant 2) (IConstant 3));
	  assertParsesTo "Do not shadow variables" "list" $ Variable "list"
  }
  
letTupleExpressions = TestCase $ do
  {
    assertParseFails "Let expression with no variables" "let () = 1 in 2";
    assertParseFails "Let expression with expression in tuple" "let (x + 1, y) = 1 in 2";
    assertParsesTo "Vanilla let expression" "let (x, y) = 1 in 2" $ LetTuple ["x", "y"] (IConstant 1) (IConstant 2)
  }
  
letRecExpressions = TestCase $ do
  {
    assertParsesTo "Vanilla let expression 1" "let rec f = 1 in 1 * 2" $ LetRec "f" [] (IConstant 1) (IBinaryExpression Multiply (IConstant 1) (IConstant 2));
    assertParsesTo "Vanilla let expression 2" "let rec f x = 2 in 3" $ LetRec "f" ["x"] (IConstant 2) (IConstant 3);
    assertParsesTo "Vanilla let expression 3" "let rec aFn xs y z aHug b d x y z = x in z" $ LetRec "aFn" ["xs", "y", "z", "aHug", "b", "d", "x", "y", "z"] (Variable "x") (Variable "z")
  }

applicationExpressions = TestCase $ do
  {
    assertParsesTo "Vanilla application 1" "dear god" $ Apply (Variable "dear") (Variable "god");
    assertParsesTo "Vanilla application 2" "rofl copter loller skates" $ Apply (Apply (Apply (Variable "rofl") (Variable "copter")) (Variable "loller")) (Variable "skates");
    assertParsesTo "Application with expressions 1" "(1 + 2) 3 blah" $ Apply (Apply (IBinaryExpression Add (IConstant 1) (IConstant 2)) (IConstant 3)) (Variable "blah");
    assertParsesTo "Application with expressions 2" "1 (2 + 3) bla_h false" $ Apply (Apply (Apply (IConstant 1) (IBinaryExpression Add (IConstant 2) (IConstant 3))) (Variable "bla_h")) (BConstant False);
    assertParsesTo "Application and arithmetic" "(fib 1) + (fib 2)" $ IBinaryExpression Add (Apply (Variable "fib") (IConstant 1)) (Apply (Variable "fib") (IConstant 2))
  }
  
conditionalExpressions = TestCase $ do
  {
    assertParsesTo "Vanilla conditional" "if 1 then 2 else 3" $ If (IConstant 1) (IConstant 2) (IConstant 3);
    assertParsesTo "Nested conditional" "if 1 then if 2 then 3 else 4 else if 5 then 6 else 7" $ If (IConstant 1) (If (IConstant 2) (IConstant 3) (IConstant 4)) (If (IConstant 5) (IConstant 6) (IConstant 7));
    assertParsesTo "Do not shadow variables" "innsbruck" $ Variable "innsbruck"
  }

tests = TestList
  [
    "Integer Constants" ~: integerConstants,
    "Real Constants" ~: realConstants,
    "Boolean Constants" ~: booleanConstants,
    "Integer Arithmetic" ~: integerArithmetic,
    "Real Arithmetic" ~: realArithmetic,
    "Mixed Arithmetic" ~: mixedArithmetic,
    "Integer < Operator" ~: lessOperator,
    "Integer <= Operator" ~: lessEqualOperator,
    "Integer > Operator" ~: greaterOperator,
    "Integer >= Operator" ~: greaterEqualOperator,
    "Equality" ~: equality,
    "Boolean Not Operator" ~: notOperator,
    "Unit" ~: unit,
    "Tuples" ~: tuples,
    "Let Value Expressions" ~: letValExpressions,
    "Let Tuple Expressions" ~: letTupleExpressions,
    "Let Rec Expressions" ~: letRecExpressions,
    "Application Expressions" ~: applicationExpressions,
    "Conditional Expressions " ~: conditionalExpressions
  ]

runTests = runTestTT tests