module Main (main) where

import           Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Options.Applicative
import           System.Environment (getArgs)
import           WCC (File, parse)

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts
  { pretty :: Bool }

opts :: Parser Opts
opts = Opts
      <$> switch
          ( long "pretty"
         <> short 'p'
         <> help "Pretty print JSON" )

main :: IO ()
main = do
  x <- getArgs
  x' <- readAll x
  BL.putStrLn (encode x')

readAll :: [String] -> IO [File]
readAll x = mapM read' x

read' :: String -> IO File
read' fp = readFile fp >>= \c -> return (parse fp c)
