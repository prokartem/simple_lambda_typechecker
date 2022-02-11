module Main where

import TypeChecker

import Options.Applicative
import Data.Semigroup ((<>))
import System.IO (openFile, IOMode (ReadMode), hGetContents)

data Sample = Sample 
    {
        hello :: String, 
        quiet :: Bool,
        enthusiasm :: Int
    }

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

-- sample :: Parser Sample
-- sample = Sample
--     <$> strOption 
--         ( long "hello"
--         <> metavar "Target"
--         <> help "Target for the greeting" )
--     <*> switch 
--         ( long "quiet"
--         <> short 'q'
--         <> help "Whether to be quiet" )
--     <*> option auto
--         ( long "enthusiasm"
--         <> help "How enthusiastically to greet"
--         <> showDefault 
--         <> value 1
--         <> metavar "INT")

opts :: ParserInfo Input
opts = info (input <**> helper)
    ( fullDesc 
    <> progDesc "Checks type for given simply typed lambda-term" )

process :: Input -> IO()
process StdInput = do
    str <- getLine
    mapM_ putStrLn (checkTypeStr str)
process (FileInput path) = do
    str <- readFile path
    mapM_ putStrLn (checkTypeStr str)

main :: IO ()
main = process =<< execParser opts 
    -- greet =<< execParser opts
    -- where
    --     opts = info (sample <**> helper)
    --         ( fullDesc 
    --         <> progDesc "Print a greeting for TARGET"
    --         <> header "hello - test for optparse-applicative" )

-- greet :: Sample -> IO()
-- greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
-- greet _ = return ()