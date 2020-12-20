module Anagram

import Data.SortedMap
import Data.SortedSet

export
AnagramDB : Type
AnagramDB = SortedMap String (SortedSet String)

export
emptyDB : AnagramDB
emptyDB = empty


normalize : String -> String
normalize = pack . sort . unpack

export
register: AnagramDB -> String -> AnagramDB
register db word =
  let key = normalize word in
  case lookup key db of
    Just set => insert key (insert word set) db
    Nothing   => insert key (insert word empty) db


export
query : AnagramDB -> String -> SortedSet String
query db word =
  let key = normalize word in
  case lookup key db of
    Just set => insert word set
    Nothing   => insert word empty
