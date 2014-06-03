import Test.Tasty
import Test.Tasty.HUnit

import System.Hclip


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]


unitTests = testGroup "Unit tests"
  [ testCase "Clear Clipboard" $ do
      setClipboard "Haskell"
      clearClipboard 
      contents <- getClipboard
      contents @?= ""

  , testCase "Set Clipboard" $ do
      clearClipboard 
      setClipboard "Haskell"
      contents <- getClipboard
      contents @?= "Haskell"

  , testCase "Get Clipboard" $ do
      setClipboard "Haskell"
      contents <- getClipboard
      contents @?= "Haskell"

  , testCase "Modify Clipboard" $ do
      let f = reverse
          s = "Haskell"
      setClipboard s
      modifyClipboard_ f
      contents <- getClipboard
      contents @?= (f s)

  , testCase "Modify Clipboard and return new contents" $ do
      let f = reverse
          s = "Haskell"
      setClipboard s
      contents <- modifyClipboard f
      contents @?= f s
  ]
