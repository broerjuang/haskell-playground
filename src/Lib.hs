module Lib
  ( program
  )
where

import           Options.Applicative

-- type alias for data Int
type ItemIndex = Int

type ItemDescription = Maybe String

data Options = Options FilePath ItemIndex ItemDescription deriving Show

defaultFilePath :: FilePath
defaultFilePath = "~/.haskell-playground.yaml"


optionParser :: Parser Options
optionParser =
  Options <$> dataPathParser <*> itemIndexParser <*> updateItemDescription

dataPathParser :: Parser FilePath
dataPathParser =
  strOption
  --supply default value if it's not given
    $  value defaultFilePath
    <> long "file-path"
    <> short 'p'
    <> metavar "FILEPATH"
    <> help ("Path to data file (default " ++ defaultFilePath ++ ")")

itemIndexParser :: Parser ItemIndex
itemIndexParser = argument auto (metavar "ITEMINDEX" <> help "index of item")

itemValueDescrption :: Parser String
itemValueDescrption = strOption
  (long "decr" <> short 'd' <> metavar "DESCRIPTION" <> help "description")


updateItemDescription :: Parser ItemDescription
updateItemDescription = Just <$> itemValueDescrption <|> flag'
  Nothing
  (long "clear-text" <> short 'c')


-- Just directly create a main file here so we don't have to it's easier to see what changes directly without changing the Main.hs

program :: IO ()
program = do
  options <- execParser (info optionParser (progDesc "Todo List Manager"))
  putStrLn $ "options : " ++ show options
  --itemIndex <- execParser (info itemIndexParser (progDesc "Todo List Manager"))
  --putStrLn $ "item index" ++ show itemIndex


