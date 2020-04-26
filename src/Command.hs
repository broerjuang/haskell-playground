module Command where

import           Options.Applicative     hiding ( infoParser )

--Type alis for our todo

type Id = Int
type Title = String
type Description = Maybe String
type Priority = Maybe String
type DueBy = Maybe String

--this one is our todo record 
data Todo = Todo
  {
    id :: Id
  , title :: Title
  , description :: Description
  , priority :: Priority
  , dueBy :: DueBy
  } deriving Show


--this one is the interface for our cli
data TodoUpdate = TodoUpdate
  { title_update :: Maybe Title
  , description_update :: Maybe Description
  , priority_update :: Maybe Priority
  , dueBy_update :: Maybe DueBy
  } deriving Show

data Command =
  Info
  | List
  | Add Todo
  | View Id
  | Update Id TodoUpdate
  | Remove Id
  deriving Show


commandParser :: Parser Command
commandParser = subparser $ mconcat
  [ command "info"   (info infoParser (progDesc "Show Info"))
  , command "list"   (info listParser (progDesc "All todos"))
  , command "add"    (info addParser (progDesc "Add todo"))
  , command "view"   (info viewParser (progDesc "show todo"))
  , command "update" (info updateParser (progDesc "Update todo"))
  , command "remove" (info removeParser (progDesc "remove todo"))
  ]


--Command Parser || Sub Command Parser

infoParser :: Parser Command
infoParser = pure Info

listParser :: Parser Command
listParser = pure List

addParser :: Parser Command
addParser = Add <$> todoParser

viewParser :: Parser Command
viewParser = View <$> indexParser

updateParser :: Parser Command
updateParser = Update <$> indexParser <*> todoUpdateParser

removeParser :: Parser Command
removeParser = Remove <$> indexParser


-- Primitive Parser 

indexParser :: Parser Id
indexParser = argument auto (metavar "TODO_ID" <> help "todo id")


titleParser :: Parser Title
titleParser =
  strOption (long "title" <> short 't' <> metavar "TITLE" <> help "todo title")

descriptionParser :: Parser Description
descriptionParser = Just <$> descriptionParser' <|> flag'
  Nothing
  (long "clear-desc" <> help "Clear description")
 where
  descriptionParser' = strOption
    (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help
      "Todo Description"
    )


priorityParser :: Parser Priority
priorityParser = Just <$> priorityParser' <|> flag' Nothing
                                                    (long "clear-priority") where
  priorityParser' =
    strOption (long "priority" <> short 'p' <> metavar "PRIORITY")

dueByParser :: Parser DueBy
dueByParser = Just <$> dueByParser' <|> flag'
  Nothing
  (long "clear-due-by" <> help "Clear due by ")
 where
  dueByParser' =
    strOption (long "due-by" <> metavar "DUE_BY" <> help "Todo Due By")

todoUpdateParser :: Parser TodoUpdate
todoUpdateParser =
  TodoUpdate
    <$> optional titleParser
    <*> optional descriptionParser
    <*> optional priorityParser
    <*> optional dueByParser

todoParser :: Parser Todo
todoParser =
  Todo
    <$> indexParser
    <*> titleParser
    <*> descriptionParser
    <*> priorityParser
    <*> dueByParser

run :: Command -> IO ()
run Info         = putStrLn "Init"
run (View index) = putStrLn $ "view with index: " ++ show index
run List         = putStrLn "Show all todos"
