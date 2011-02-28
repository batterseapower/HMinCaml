-----------------------------------------------------------------------------------------
{-| Module      : Tests.KNormalize
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.KNormalize where

import Test.HUnit
import Control.Monad(when)

import HMinCaml.Common
import HMinCaml.KNormalize
import qualified HMinCaml.Simplify as S

assertNormalizesTo :: String -> S.Syntax -> Syntax -> Assertion
assertNormalizesTo message parseSyntax normalizedSyntax = assertEqual message (Right normalizedSyntax) (runCompilerError $ kNormalize parseSyntax)

assertNormalizationFails :: String -> S.Syntax -> Assertion
assertNormalizationFails message syntax = when (fails (runCompilerError $ kNormalize syntax)) (assertFailure $ message ++ ": parse did not fail!")
  where
    fails = either (const False) (const True)

unboundVariables = TestCase $ do
  assertNormalizationFails "Simple unbound variable" (S.Variable "x")
  assertNormalizationFails "Unbound variable in value of let" (S.LetVal "x" (S.Variable "x") S.Unit)
  assertNormalizationFails "Unbound variable in tuple value of let" (S.LetTuple ["x"] (S.Variable "x") S.Unit)
  assertNormalizationFails "Unbound variable in body of recursive let" (S.LetRec "xf" ["x"] (S.Unit) (S.Variable "x"))

simpleNormalizations = TestCase $ do
  assertNormalizesTo "Unit" S.Unit Unit
  assertNormalizesTo "Integer constants" (S.IConstant 1) 
                                         (IConstant 1)
  assertNormalizesTo "Floating point constants" (S.FConstant 1.0) 
                                                (FConstant 1.0)
  assertNormalizesTo "Boolean constants" (S.BConstant True) 
                                         (BConstant True)
  assertNormalizesTo "Tuples" (S.Tuple [S.IConstant 1, S.Unit]) 
                              (LetVal "1" (IConstant 1) (LetVal "2" Unit (Tuple ["1", "2"])))
  assertNormalizesTo "Integer negation" (S.INegate S.Unit) 
                                        (LetVal "1" Unit (INegate "1"))
  assertNormalizesTo "Integer expressions" (S.IBinaryExpression Add (S.IConstant 0) (S.BConstant True)) 
                                           (LetVal "1" (IConstant 0) (LetVal "2" (BConstant True) (IBinaryExpression Add "1" "2")))
  assertNormalizesTo "Floating point negation" (S.FNegate S.Unit) 
                                               (LetVal "1" Unit (FNegate "1"))
  assertNormalizesTo "Floating point expressions" (S.FBinaryExpression Add (S.FConstant 0) (S.IConstant 2)) 
                                                  (LetVal "1" (FConstant 0) (LetVal "2" (IConstant 2) (FBinaryExpression Add "1" "2")))
  assertNormalizesTo "Boolean negation" (S.BNot S.Unit) 
                                        (LetVal "1" Unit (BNot "1"))
  assertNormalizesTo "Boolean equality" (S.BEqual (S.FConstant 0) (S.BConstant True)) 
                                        (LetVal "1" (FConstant 0) (LetVal "2" (BConstant True) (BEqual "1" "2")))
  assertNormalizesTo "Boolean less equality" (S.BLessEqual (S.FConstant 0) (S.BConstant True)) 
                                             (LetVal "1" (FConstant 0) (LetVal "2" (BConstant True) (BLessEqual "1" "2")))
  assertNormalizesTo "If expressions" (S.If (S.IConstant 1) (S.Unit) (S.FConstant 1.0)) 
                                      (LetVal "1" (IConstant 1) (If "1" (Unit) (FConstant 1.0)))
  
complexNormalizations = TestCase $ do
  assertNormalizesTo "Let value expressions" (S.LetVal "x" S.Unit (S.IConstant 1)) 
                                             (LetVal "x.1" Unit (IConstant 1))
  assertNormalizesTo "Let value expressions with variable" (S.LetVal "x" S.Unit (S.Variable "x")) 
                                                           (LetVal "x.1" Unit (Variable "x.1"))
  assertNormalizesTo "Let tuple expressions" (S.LetTuple ["x", "y", "z"] (S.BConstant False) S.Unit)
                                             (LetVal "4" (BConstant False) (LetTuple ["x.1", "y.2", "z.3"] "4" Unit))
  assertNormalizesTo "Let tuple expressions with variable" (S.LetTuple ["x", "y"] (S.Unit) (S.Tuple [S.Variable "x", S.Variable "y"]))
                                                           (LetVal "3" Unit (LetTuple ["x.1", "y.2"] "3" (LetVal "4" (Variable "x.1") (LetVal "5" (Variable "y.2") (Tuple ["4", "5"])))))
  assertNormalizesTo "Let tuple expressions preserve bindings in body" 
                                                           (S.LetVal "w" S.Unit (S.LetTuple ["x", "y"] (S.Unit) (S.Variable "w")))
                                                           (LetVal "w.1" Unit (LetVal "4" Unit (LetTuple ["x.2", "y.3"] "4" (Variable "w.1"))))
  assertNormalizesTo "Recursive let expressions" (S.LetRec "x" ["y", "z"] (S.Unit) (S.IConstant 5))
                                                 (LetRec "x.1" ["y.2", "z.3"] Unit (IConstant 5))
  assertNormalizesTo "Recursive let expressions with variable" (S.LetRec "x" ["y", "z"] (S.Tuple [S.Variable "y", S.Variable "z"]) (S.Variable "x"))
                                                               (LetRec "x.1" ["y.2", "z.3"] (LetVal "4" (Variable "y.2") (LetVal "5" (Variable "z.3") (Tuple ["4", "5"]))) (Variable "x.1"))
  assertNormalizesTo "Recursive let expressions with recursion" (S.LetRec "x" ["y"] (S.Apply (S.Variable "x") (S.IConstant 1)) (S.Unit))
                                                                (LetRec "x.1" ["y.2"] (LetVal "3" (Variable "x.1") (LetVal "4" (IConstant 1) (Apply "3" "4"))) Unit)
  
tests = TestList
  [
    "Unbound variables" ~: unboundVariables,
    "Simple normalizations" ~: simpleNormalizations,
    "Variable introducing normalizations" ~: complexNormalizations
  ]

runTests = runTestTT tests