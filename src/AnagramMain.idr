module Main

import Anagram
import Data.SortedSet

importFromFile : (filename: String) -> IO (Either FileError AnagramDB)
importFromFile filename = do
  Right file <- openFile filename Read
    | Left e => pure (Left e)
  loop file emptyDB
where
  loop : File -> AnagramDB -> IO (Either FileError AnagramDB)
  loop file db = do
    isEOF <- fEOF file
    if isEOF
    then pure $ Right db
    else do
      Right word <- fGetLine file
        | Left e => pure (Left e)
      let db = register db (trim word)
      loop file db


showResult: AnagramDB -> String -> IO ()
showResult db word =
  let anagrams = Anagram.query db word in
  printLn $ SortedSet.toList anagrams


main : IO ()
main = do
  [_, key] <- getArgs
  Right db <- importFromFile "/usr/share/dict/words"
  showResult db key
