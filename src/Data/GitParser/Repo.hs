module Data.GitParser.Repo where
import Data.GitParser.Types
import Data.Maybe
import System.FilePath
import Data.Char
import System.Directory
import Control.Monad
import qualified Data.ByteString.Char8 as C

-- TODO: This is some ugly stuff.

strip = f . f
    where f = C.reverse . C.dropWhile isSpace 

getPackedRefs path = do 
  f <- fmap C.lines $ C.readFile (path </> "packed-refs")
  return . catMaybes $ map parseLine f
    where parseLine x = let cl   = C.takeWhile (/= '#') x
                            sha  = C.takeWhile (/= ' ') cl
                            name =  strip . C.dropWhile (/= ' ') $ cl
                        in if C.null name || C.null sha
                             then Nothing
                             else Just $ GitBranch (C.unpack $ ff name) (C.unpack sha)
                        where f = C.tail . C.dropWhile (/= '/') 
                              ff = f . f

getLocalRefHeads path = do 
  c <- getDirectoryContents (path </> refs)
  f <- filterM (\x->doesFileExist (path </> refs </> x)) c
  mapM readContents f
    where readContents x = do s <- C.readFile (path </> refs </> x)
                              return $ GitBranch x (C.unpack . strip $ s)
          refs = "refs/heads"
  
readHead :: FilePath -> [GitBranch] -> IO SHA
readHead path branches = do 
  h <- C.readFile $ path </> "HEAD"
  if C.isPrefixOf (C.pack "ref:") h
     then let p = parseRef h in return (lookupHead p)
     else return $ C.unpack h

  where lookupHead h = getBranchHEAD 
                       . head 
                       . filter (\x-> getBranchName x == h) $ branches 
        parseRef = C.unpack
                  . strip 
                  . C.reverse 
                  . C.takeWhile (/= '/') 
                  . C.reverse 
                  . C.dropWhile (/= ':')

repoFromPath path = do 
  rbranches <- getPackedRefs path
  lbranches <- getLocalRefHeads path
  let branches = rbranches ++ lbranches
  head <- readHead path branches
  return (GitRepo path [RemoteRepo ""] branches head) 

getCurrentBranch repo = let b = lookupBranch 
                        in if null b then getRepoHEAD repo else getBranchName (head b)
    where lookupBranch = filter (\x-> getBranchHEAD x == getRepoHEAD repo) $ getBranches repo