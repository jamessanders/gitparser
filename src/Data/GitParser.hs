module Data.GitParser 
    (GitCommit (..)
    ,GitField (..)
    ,parseCommits)
where
import Control.Monad.State
import Data.Char
import Data.List
import Data.Time.Format 
import Data.Time.LocalTime
import System.Locale 

data GitField = SHA String | Author String | Date String | Message String 
              deriving Show

data CommitAuthor = CommitAuthor { getName :: String, getEmail :: String } deriving (Show,Read,Eq,Ord)

data GitCommit = GitCommit { getSHA     :: String
                           , getAuthor  :: CommitAuthor
                           , getDate    :: Maybe LocalTime
                           , getMessage :: String } deriving (Show,Read)

emptyCommit = GitCommit "" (CommitAuthor "Unknown" "Unknown")  Nothing ""

data ParserState = PS { psLeft   :: [String]
                      , psRight  :: [String]
                      , psBlocks :: [GitField] }
                 deriving (Show)

type GitParser a = State ParserState a

mkParserState c = PS [] (lines c) []

getRight,getLeft :: GitParser [String]
getRight = fmap psRight get
getLeft  = fmap psLeft  get

putBlock bl = do ps <- get
                 put $ ps { psBlocks = psBlocks ps ++ [bl] }

getNextLine :: GitParser String
getNextLine = do r <- getRight
                 if null r then return [] else return (head r)


shift = do ps <- get
           if null (psRight ps)
              then return ()
              else do
                nl <- getNextLine
                put $ ps { psLeft = psLeft ps ++ [nl], psRight = tail $ psRight ps }

clear = do ps <- get
           put $ ps { psLeft = [] }

parseCommit :: GitParser ()
parseCommit = do nl <- getRight
                 if null nl 
                   then return () 
                   else do getNextLine >>= findNext 
                           parseCommit
    where
      findNext x | "commit"  `isPrefixOf` x = parseSHA 
                 | "Merge:"  `isPrefixOf` x = parseMerge
                 | "Author:" `isPrefixOf` x = parseAuthor
                 | "Date:"   `isPrefixOf` x = parseDate
                 | "    "    `isPrefixOf` x = parseMessage
      findNext x = shift >> clear
                
parseSHA = do r <- getNextLine
              let sha = takeWhile (not . isSpace) . tail . dropWhile (/= ' ') $ r
              putBlock (SHA sha)
              shift >> clear

colonParse x  = takeWhile (/= '\n') . tail . dropWhile (/= ':') $ x
colonBlock con = do r <- getNextLine
                    putBlock (con $ strip . colonParse $ r)
                    shift >> clear
 
            
parseMerge   = shift >> clear
parseAuthor  = colonBlock Author
parseDate    = colonBlock Date

parseMessage = do r <- getNextLine
                  l <- getLeft
                  if "    " `isPrefixOf` r
                    then shift >> parseMessage
                    else putBlock (Message $ unlines . map (drop 4) $ l) >> clear

strip = reverse . fn . reverse . fn
        where fn = dropWhile isSpace


splitCommits l = filter (not . null) $  splitCommits (lines l) [] []
               where
                 splitCommits [] ls final = final ++ [unlines ls]
                 splitCommits (x:xs) ls final = if "commit" `isPrefixOf` x
                                                  then splitCommits xs [x] (final ++ [unlines ls])
                                                  else splitCommits xs (ls ++ [x]) final


toCommit = foldl af emptyCommit
    where af c (SHA a)     = c { getSHA     = a }
          af c (Author a)  = c { getAuthor  = toAuthor a }
          af c (Date a)    = c { getDate    = toLocalTime a }
          af c (Message a) = c { getMessage = a }

toLocalTime = parseTime defaultTimeLocale "%a %b %e %H:%M:%S %Y %z" 
toAuthor x = let name = strip . takeWhile (/= '<') $ x
                 emal = tail . takeWhile (/= '>') . dropWhile (/= '<') $ x
             in CommitAuthor name emal

parseCommits = map (toCommit . psBlocks . execState parseCommit . mkParserState) . splitCommits