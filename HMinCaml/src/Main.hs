-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Main where

{-
import Test.HUnit

import qualified Tests.Parser as Parser
import qualified Tests.Simplify as Simplify
import qualified Tests.KNormalize as KNormalize
import qualified Tests.InferTypes as InferTypes
import qualified Tests.ClosureConversion as ClosureConversion
import qualified Tests.VariableLowering as VariableLowering
import qualified Tests.GenerateCode as GenerateCode


tests = TestList
  [
    Parser.tests,
    Simplify.tests,
    KNormalize.tests,
    InferTypes.tests,
    ClosureConversion.tests,
    VariableLowering.tests--,
    --GenerateCode.tests
  ]

main = runTestTT tests
-}


import HMinCaml.Common
import qualified HMinCaml.Parser as Parser
import qualified HMinCaml.Simplify as Simplify
import qualified HMinCaml.KNormalize as KNormalize
import qualified HMinCaml.InferTypes as InferTypes
import qualified HMinCaml.ClosureConversion as ClosureConversion
import qualified HMinCaml.VariableLowering as VariableLowering
import qualified HMinCaml.GenerateCode as GenerateCode

import System(getArgs, getProgName)

pipeline :: String -> CompilerError String
pipeline program = return program
           >>= Parser.parse 
           >>= Simplify.simplify 
           >>= KNormalize.kNormalize 
           >>= InferTypes.inferTypes 
           >>= ClosureConversion.closureConvert 
           >>= VariableLowering.lowerVariables 
           >>= GenerateCode.generateCode

main = do
  args <- getArgs
  case args of
    [file] -> do
      contents <- readFile file
      case (runCompilerError (pipeline contents)) of
        Left error -> putStrLn $ "Compiler error: " ++ error
        Right output -> writeFile (file ++ ".asm") output
    otherwise -> do
      program <- getProgName
      putStrLn $ "Usage:\n\t" ++ program ++ " <source file>"
