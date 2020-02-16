module Repl (repl) where

import           Config
import           Database as DB
import qualified Client as Client
import qualified Data as Data
import           Data.Time.Clock (getCurrentTime)
import           Data.List

data Command = ShowFeed | AddPost String | GetPosts | ReadInbox | Exit deriving (Eq, Show)

parse :: String -> Either String Command
parse "feed" = Right ShowFeed
parse "posts" = Right GetPosts
parse "inbox" = Right ReadInbox
parse "exit" = Right Exit
parse withArg = case (words withArg) of
  "addPost" : remaining -> Right $ AddPost $ intercalate " " remaining
  _ -> Left $ withArg ++ " is an unkown command"

process :: Config -> Command -> IO ()
process config ShowFeed = DB.listActors config >>= Client.feed >>= print
process config (AddPost body) = fmap (\t -> Data.Post body "me" t) getCurrentTime >>= DB.addPost config
process config GetPosts = (DB.getPosts config) >>= print
process config ReadInbox = (DB.readInbox config) >>= print
process _      Exit = error "Bye!"

repl :: Config -> IO ()
repl config = do
  line <- getLine
  case (parse line) of
    Right command -> process config command
    Left error -> putStrLn error
