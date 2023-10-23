{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
module WCC
    ( File
    , parse
    ) where

import Data.Aeson (ToJSON)
import Data.List (group, maximumBy)
import Data.Ord (comparing)
import GHC.Generics

-------------------------------------------------------------------------------
-- MostRepeatedChar
-------------------------------------------------------------------------------

data MostRepeatedChar = MostRepeatedChar
    { char  :: Char
    , count :: Int
    } deriving (Show, Generic)

instance ToJSON MostRepeatedChar

-------------------------------------------------------------------------------
-- MostRepeatedWord
-------------------------------------------------------------------------------

data MostRepeatedWord = MostRepeatedWord
    { word  :: String
    , count :: Int
    } deriving (Show, Generic)

instance ToJSON MostRepeatedWord

-------------------------------------------------------------------------------
-- File
-------------------------------------------------------------------------------

data File = File
    { filepath           :: String
    , char_count         :: Int
    , word_count         :: Int
    , line_count         :: Int
    , most_repeated_char :: MostRepeatedChar
    , most_repeated_word :: MostRepeatedWord
    }  deriving (Show, Generic)

instance ToJSON File

parse :: String -> String -> File
parse fp c = File
    { filepath           = fp
    , char_count         = length c
    , word_count         = length w
    , line_count         = length (lines c)
    , most_repeated_char = mostRepeatedChar c
    , most_repeated_word = mostRepeatedWord w
    }
  where w = words c

mostRepeatedChar :: String -> MostRepeatedChar
mostRepeatedChar c = MostRepeatedChar
    { char  = head g
    , count = length g
    }
  where g = maximumBy (comparing length) (group c)

mostRepeatedWord :: [String] -> MostRepeatedWord
mostRepeatedWord w = MostRepeatedWord
    { word  = head g
    , count = length g
    }
  where g = maximumBy (comparing length) (group w)
