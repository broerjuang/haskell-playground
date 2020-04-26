module Lib
  ( program
  )
where

import           Options.Applicative     hiding ( infoParser )
import           Command                       as C

data Options = Options FilePath Command deriving Show

defaultFilePath :: FilePath
defaultFilePath = "~/.haskell-playground.yaml"


optionParser :: Parser Options
optionParser = Options <$> dataPathParser <*> commandParser

dataPathParser :: Parser FilePath
dataPathParser =
  strOption
  --supply default value if it's not given
    $  value defaultFilePath
    <> long "file-path"
    <> short 'p'
    <> metavar "FILEPATH"
    <> help ("Path to data file (default " ++ defaultFilePath ++ ")")


-- Just directly create a main file here so we don't have to it's easier to see what changes directly without changing the Main.hs
--

program :: IO ()
program = do
  Options _filePath command <- execParser
    (info optionParser (progDesc "Todo List Manager"))
  putStrLn $ show command
  --putStrLn $ "options : " ++ show options
  --itemIndex <- execParser (info itemIndexParser (progDesc "Todo List Manager"))
  --putStrLn $ "item index" ++ show itemIndex


