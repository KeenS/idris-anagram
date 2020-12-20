module Tests.Anagram

import Test.Unit
import Anagram
import System
import Data.SortedSet


exitIfFail : IO (List Bool) -> IO ()
exitIfFail action = do
  results <- action
  if not (all id results)
  then do
    putStrLn "Some tests failed"
    exit 1
  else pure ()

testEmptyQuery : IO Bool
testEmptyQuery = do
  let result = query emptyDB "hoge"
  assertTrue $ contains "hoge" result

testRegisterQuery : IO Bool
testRegisterQuery = do
  let db = register emptyDB "eat"
  let result = query db "tea"
  assertTrue $ contains "tea" result

export
test : IO ()
test = exitIfFail $ runTests [
  testEmptyQuery,
  testRegisterQuery
]

