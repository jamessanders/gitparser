
data GitField = SHA String | Author String | Data String | Message String 
              deriving Show

data ParserState = PS { psLeft   :: [String]
                      , psRight  :: [String]
                      , psBlocks :: [GitField] }
                 deriving (Show)

getRight = fmap psRight get
getLeft  = fmap psLeft  get

putBlock bl = do ps <- get
              put $ ps { psBlocks = psBlocks ps ++ bl }

getNextLine = do r <- getRight
                 return (head r)


shift = do ps <- get
           nl <- getNextLine
           put $ ps { psLeft = psLeft ps ++ nl }

clear = do ps <- get
           put $ ps { psLeft = [] }

parseCommit = do getNextLine >>= findNext
    where
      findNext x | "commit"  `isInfixOf` x = parseSHA 
                 | "Merge:"  `isInfixOf` x = parseMerge
                 | "Author:" `isInfixOf` x = parseAuthor
                 | "Date:"   `isInfixOf` x = parseDate
                 | "\t"      `isInfixOf` x = parseMessage
                
parseSHA = do r <- getNextLine
              let sha = takeWhile (not . isSpace) . tail . dropWhile (/= ' ') $ r
              putBlock (SHA sha)
              shift >> clear
              

