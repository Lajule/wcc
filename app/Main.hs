{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

-- stack --allow-different-user script --resolver lts-21.14 --package base --package bytestring --package aeson


import           Data.Aeson (encode, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List (group, maximumBy)
import           Data.Ord (comparing)
import           GHC.Generics
import           System.Environment (getArgs)
import Lib

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

main :: IO ()
main = do
  x <- getArgs
  x' <- readAll x
  BL.putStrLn (encode x')

readAll :: [String] -> IO [File]
readAll x = mapM read' x

read' :: String -> IO File
read' fp = readFile fp >>= \c -> return (parse fp c)

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
