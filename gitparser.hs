import Control.Monad.State
import Data.List
import Data.Char

data GitField = SHA String | Author String | Date String | Message String 
              deriving Show

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
      findNext x | "commit"  `isInfixOf` x = parseSHA 
                 | "Merge:"  `isInfixOf` x = parseMerge
                 | "Author:" `isInfixOf` x = parseAuthor
                 | "Date:"   `isInfixOf` x = parseDate
                 | "    "    `isInfixOf` x = parseMessage
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
                  if "    " `isInfixOf` r
                    then shift >> parseMessage
                    else putBlock (Message $ unlines . map (drop 4) $ l) >> clear

strip = reverse . fn . reverse . fn
        where fn = dropWhile isSpace