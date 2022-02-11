module Main where

import TypeChecker

import Options.Applicative
    ( (<**>),
      Alternative((<|>)),
      flag',
      fullDesc,
      help,
      info,
      long,
      metavar,
      progDesc,
      short,
      strOption,
      execParser,
      helper,
      Parser,
      ParserInfo )
import Data.Semigroup ((<>))
import System.IO () 

data Input =
    FileInput FilePath 
    | StdInput

fileInput :: Parser Input
fileInput = FileInput <$> strOption 
    ( long "file"
    <> short 'f'
    <> metavar "FILENAME"
    <> help "Input file")

stdInput :: Parser Input
stdInput = flag' StdInput 
    ( long "stdin"
    <> help "Read from stdin")

input :: Parser Input
input = fileInput <|> stdInput

process :: Input -> IO()
process StdInput = do
    str <- getLine
    mapM_ putStrLn (checkTypeStr str)
process (FileInput path) = do
    str <- readFile path
    mapM_ putStrLn (checkTypeStr str)

main :: IO ()
main = process =<< execParser opts
    where
        opts :: ParserInfo Input
        opts = info (input <**> helper)
            ( fullDesc 
            <> progDesc "Checks type for given simply typed lambda-term" )
