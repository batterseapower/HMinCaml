-----------------------------------------------------------------------------------------
{-| Module      : Tests.Simplify
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.Simplify where

import Test.HUnit

import HMinCaml.Common
import HMinCaml.Simplify
import qualified HMinCaml.Parser as P

assertSimplifiesTo :: String -> P.Syntax -> Syntax -> Assertion
assertSimplifiesTo message parseSyntax simplifiedSyntax = assertEqual message (Right simplifiedSyntax) (runCompilerError $ simplify parseSyntax)


simplifications = TestCase $ do
  assertSimplifiesTo "Less" (P.BBinaryExpression Less (P.IConstant 1) (P.IConstant 2)) 
                            (BNot (BLessEqual (IConstant 2) (IConstant 1)))
  assertSimplifiesTo "LessEqual" (P.BBinaryExpression LessEqual (P.IConstant 1) (P.IConstant 2)) 
                                 (BLessEqual (IConstant 1) (IConstant 2))
  assertSimplifiesTo "Greater" (P.BBinaryExpression Greater (P.IConstant 1) (P.IConstant 2)) 
                               (BNot (BLessEqual (IConstant 1) (IConstant 2)))
  assertSimplifiesTo "GreaterEqual" (P.BBinaryExpression GreaterEqual (P.IConstant 1) (P.IConstant 2)) 
                                    (BLessEqual (IConstant 2) (IConstant 1))
  assertSimplifiesTo "Equal" (P.BBinaryExpression Equal (P.IConstant 1) (P.IConstant 2)) 
                             (BEqual (IConstant 1) (IConstant 2))
  
tests = TestList
  [
    "All Simplifications" ~: simplifications
  ]

runTests = runTestTT tests