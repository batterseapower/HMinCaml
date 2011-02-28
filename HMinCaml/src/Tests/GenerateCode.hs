-----------------------------------------------------------------------------------------
{-| Module      : Tests.GenerateCode
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Tests.GenerateCode where

import Test.HUnit
import Char(toLower, isSpace)
import List(elemIndices)
import System.Directory(getDirectoryContents)
import System.IO(hGetContents)
import System.Process(runInteractiveProcess, waitForProcess)

import HMinCaml.Common
import HMinCaml.GenerateCode
import qualified HMinCaml.VariableLowering as V

assertSimplifiesTo :: String -> V.Program -> String -> Assertion
assertSimplifiesTo message loweredSyntax output = assertEqual message (Right output) (runCompilerError $ generateCode loweredSyntax)

testDirectory = "E:\\Programming\\Current\\HMinCaml\\HMinCaml\\Functional Tests\\"
testHarnessFor file = runInteractiveProcess "cmd" ["/C", testDirectory ++ "Build.bat", file, file ++ ".asm", file ++ ".exe"] Nothing Nothing

runFunctionalTests = TestCase $ do
    files <- getDirectoryContents testDirectory
    let testFiles = [file | file <- files, file `hasExtension` "minml"]
    mapM_ (\file -> runFunctionalTest file (testDirectory ++ file)) testFiles
  where
    stringToLower :: String -> String
    stringToLower = map toLower
   
    hasExtension :: FilePath -> String -> Bool
    hasExtension file extension = case (extensionOf file) of
                                  Nothing -> False
                                  Just fileExtension -> (stringToLower fileExtension) == (stringToLower extension)
  
    extensionOf :: FilePath -> Maybe FilePath
    extensionOf path =
      let is = elemIndices '.' path in
      if is == [] 
      then Nothing 
      else (Just . tail . snd) (splitAt (last is) path)

runFunctionalTest :: String -> FilePath -> Assertion
runFunctionalTest programName programPath = do
    contents <- readFile programPath
    (_, stdout, _, harness) <- testHarnessFor programPath
    
    waitForProcess harness
    actualResult <- hGetContents stdout
    
    assertEqual programName (getExpectedResult contents) ((last . lines) actualResult)
  where
    dropUntilSpace = dropWhile (/= ' ')
    
    getExpectedResult = reverse . tail . dropUntilSpace . reverse . tail . dropUntilSpace . head . lines

  
tests = TestList
  [
    "All Functional Tests" ~: runFunctionalTests
  ]

runTests = runTestTT tests