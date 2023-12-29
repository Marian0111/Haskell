module Main where


import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude
import Data.IntMap (mapMaybe)

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do
  let emptyDB = DB.empty
  result <- DB.save emptyDB
  case result of
    Error _ -> putStrLn "Error initializing the database."
    Success _ -> putStrLn "Database initialized successfully."


-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do
  dbResult <- DB.load
  case dbResult of
    Error _ -> putStrLn "Error loading the database."
    Success db -> do
      let entryIdToFind = getOptId getOpts
      case DB.findFirst (\entry -> entryIdToFind == Entry.Entry.entryId entry) db of
        Nothing -> putStrLn "Entry not found."
        Just entry -> putStrLn $ L.intercalate "\n"
          [ "[" ++ show entryIdToFind ++ "] " ++ Entry.Entry.entryFilename entry ++ ":",
            "   Description: " ++ Entry.Entry.entryDescription entry,
            "   Tags: " ++ unwords (Entry.Entry.entryTags entry),
            "   First line: " ++ head (lines (Entry.Entry.entrySnippet entry))
          ]


-- -- | Handle the get command
-- handleGet :: TestableMonadIO m => GetOptions -> m ()
-- handleGet getOpts = do
--   dbResult <- DB.load
--   case dbResult of
--     Error _ -> putStrLn "Error loading the database."
--     Success db -> do
--       let entryIdToFind = getOptId getOpts
--       case DB.findFirst (\entry -> entryIdToFind == Entry.entryId entry) db of
--         Nothing -> putStrLn "Entry not found."
--         Just entry -> putStrLn (show (FmtEntry entry))


-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do
  dbResult <- DB.load
  case dbResult of
    Error _ -> putStrLn "Error loading the database."
    Success db -> do
      let queries = searchOptTerms searchOpts
          matchingEntries = DB.findAll (matchedByAllQueries queries) db
      if null matchingEntries
        then putStrLn "No entries found."
        else mapM_ (putStrLn . show . FmtEntry) matchingEntries

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts =
  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
