{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WCC
  ( File(..),
    MostRepeatedWord(..),
    parse,
  )
where

import Data.Aeson (ToJSON)
import Data.List (group, maximumBy)
import Data.Ord (comparing)
import GHC.Generics
import Prelude as P

-------------------------------------------------------------------------------
-- MostRepeatedWord
-------------------------------------------------------------------------------

data MostRepeatedWord = MostRepeatedWord
  { word :: String,
    word_count :: Int
  }
  deriving (Show, Generic, Eq)

instance ToJSON MostRepeatedWord

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

data File = File
  { filepath :: String,
    bytes :: Int,
    words :: Int,
    newlines :: Int,
    most_repeated_word :: MostRepeatedWord
  }
  deriving (Show, Generic, Eq)

instance ToJSON File

parse :: String -> String -> File
parse fp c =
  File
    { filepath = fp,
      bytes = length c,
      words = length w,
      newlines = length (lines c),
      most_repeated_word = mostRepeatedWord w
    }
  where
    w = P.words c

mostRepeatedWord :: [String] -> MostRepeatedWord
mostRepeatedWord w =
  MostRepeatedWord
    { word = head g,
      word_count = length g
    }
  where
    g = maximumBy (comparing length) (group w)
