module Data.GitParser.Types where

import Data.Time.LocalTime
import Data.Time.Format 
import Control.Monad.State

type Name = String
type SHA  = String

data GitBranch = GitBranch { getBranchName :: Name
                           , getBranchHEAD :: SHA }
                 deriving Show

newtype RemoteRepo = RemoteRepo String deriving (Show)

data GitRepo = GitRepo { getLocalPath :: FilePath 
                       , getRemotes   :: [RemoteRepo]
                       , getBranches  :: [GitBranch]
                       , getRepoHEAD  :: SHA }
               deriving Show

data ParseField = SHA SHA | Author String | Date String | Message String 
                  deriving Show

data CommitAuthor = CommitAuthor { getName :: String, getEmail :: String } deriving (Show,Read,Eq,Ord)

data GitCommit = GitCommit { getSHA     :: SHA
                           , getAuthor  :: CommitAuthor
                           , getDate    :: Maybe LocalTime
                           , getMessage :: String } deriving (Show,Read)

emptyCommit = GitCommit "" (CommitAuthor "Unknown" "Unknown")  Nothing ""

data ParserState = PS { psLeft   :: [String]
                      , psRight  :: [String]
                      , psBlocks :: [ParseField] }
                 deriving (Show)

type GitParser a = State ParserState a

mkParserState c = PS [] (lines c) []
