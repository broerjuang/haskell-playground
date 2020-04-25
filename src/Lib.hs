module Lib
  ( program
  )
where

import           Options.Applicative     hiding ( infoParser )

-- type alias for data Int
type ItemIndex = Int
type ItemTitle = String
type ItemPriority = Maybe String
type ItemDueBy = Maybe String
type ItemDescription = Maybe String

data ItemUpdate = ItemUpdate
  { titleUpdate :: Maybe ItemTitle
  , descriptionUpdate :: Maybe ItemDescription
  , priorityUpdate :: Maybe ItemPriority
  , dueByUpdate :: Maybe ItemDueBy
  } deriving Show

data Command =
  Info
  | Init
  | List
  | Add
  | View
  | Update ItemIndex ItemUpdate
  | Remove deriving Show

infoParser :: Parser Command
infoParser = pure Info

initParser :: Parser Command
initParser = pure Init

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = pure Add

viewParser :: Parser Command
viewParser = pure View

updateParser :: Parser Command
updateParser = Update <$> itemIndexParser <*> updateItemParser

updateItemParser :: Parser ItemUpdate
updateItemParser =
  ItemUpdate
    <$> optional titleUpdateParser
    <*> optional descriptionUpdateParser
    <*> optional priorityUpdateParser
    <*> optional dueByUpdateParser

titleUpdateParser :: Parser ItemTitle
titleUpdateParser = undefined

descriptionUpdateParser :: Parser ItemDescription
descriptionUpdateParser = undefined

priorityUpdateParser :: Parser ItemPriority
priorityUpdateParser = undefined

dueByUpdateParser :: Parser ItemDueBy
dueByUpdateParser = undefined

removeParser :: Parser Command
removeParser = pure Remove

commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info"   (info infoParser (progDesc "Show Info"))
  , command "init"   (info initParser (progDesc "init todo store"))
  , command "list"   (info listParser (progDesc "Show all todos"))
  , command "add"    (info addParser (progDesc "Add todo"))
  , command "view"   (info initParser (progDesc "view todo"))
  , command "update" (info initParser (progDesc "update todo"))
  , command "remove" (info removeParser (progDesc "remove todo"))
  ]

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
--

run :: FilePath -> Command -> IO ()
run _ Info = putStrLn "info from run"
run _ Init = putStrLn "init from run"
run _ List = putStrLn "list from run"
run _ View = putStrLn "view from run"
run _ Add  = putStrLn "add from run"
run _ (Update index itemUpdate) =
  putStrLn
    $  "update index :"
    ++ show index
    ++ "update item_update: "
    ++ show itemUpdate
run _ Remove = putStrLn "remove from run"

program :: IO ()
program = do
  Options filePath command <- execParser
    (info optionParser (progDesc "Todo List Manager"))
  run filePath command
  --putStrLn $ "options : " ++ show options
  --itemIndex <- execParser (info itemIndexParser (progDesc "Todo List Manager"))
  --putStrLn $ "item index" ++ show itemIndex


