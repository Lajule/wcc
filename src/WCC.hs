{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module WCC
  ( File,
    parse,
  )
where

import Data.Aeson (ToJSON)
import Data.List (group, maximumBy)
import Data.Ord (comparing)
import GHC.Generics
import Prelude as P

-------------------------------------------------------------------------------
-- MostRepeatedChar
-------------------------------------------------------------------------------

data MostRepeatedChar = MostRepeatedChar
  { byte :: Char,
    count :: Int
  }
  deriving (Show, Generic)

instance ToJSON MostRepeatedChar

-------------------------------------------------------------------------------
-- MostRepeatedWord
-------------------------------------------------------------------------------

data MostRepeatedWord = MostRepeatedWord
  { word :: String,
    count :: Int
  }
  deriving (Show, Generic)

instance ToJSON MostRepeatedWord

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

data File = File
  { filepath :: String,
    bytes :: Int,
    words :: Int,
    newlines :: Int,
    most_repeated_byte :: MostRepeatedChar,
    most_repeated_word :: MostRepeatedWord
  }
  deriving (Show, Generic)

instance ToJSON File

parse :: String -> String -> File
parse fp c =
  File
    { filepath = fp,
      bytes = length c,
      words = length w,
      newlines = length (lines c),
      most_repeated_byte = mostRepeatedChar c,
      most_repeated_word = mostRepeatedWord w
    }
  where
    w = P.words c

mostRepeatedChar :: String -> MostRepeatedChar
mostRepeatedChar c =
  MostRepeatedChar
    { byte = head g,
      count = length g
    }
  where
    g = maximumBy (comparing length) (group c)

mostRepeatedWord :: [String] -> MostRepeatedWord
mostRepeatedWord w =
  MostRepeatedWord
    { word = head g,
      count = length g
    }
  where
    g = maximumBy (comparing length) (group w)
