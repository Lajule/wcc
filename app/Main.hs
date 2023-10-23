module Main (main) where

import           Data.Aeson (encode)
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import           Options.Applicative
import           WCC (File, parse)

-------------------------------------------------------------------------------
-- Opts
-------------------------------------------------------------------------------

data Opts = Opts Bool [String]

main :: IO ()
main = wcc =<< execParser x
  where
    x = info (opts <**> helper)
      ( fullDesc
     <> progDesc "Print newline, word, and byte counts for each FILE, and a total line if more than one FILE is specified."
     <> header "wcc - print newline, word, and byte counts for each file" )

opts :: Parser Opts
opts = Opts
      <$> switch
          ( long "pretty"
         <> short 'p'
         <> help "Pretty print JSON" )
      <*> many (argument str (metavar "FILES..."))

wcc :: Opts -> IO ()
wcc (Opts b x) = do
  x' <- readAll x
  BL.putStrLn (encode' b x')

readAll :: [String] -> IO [File]
readAll x = mapM read' x

read' :: String -> IO File
read' fp = readFile fp >>= \c -> return (parse fp c)

encode' :: Bool -> [File] -> BL.ByteString
encode' False x = encode x
encode' True x = encodePretty x
