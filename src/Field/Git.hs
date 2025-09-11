module Field.Git
( GitCommitContent (..)
, commitField
, headCommitField
) where

import Hakyll
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Exit (ExitCode(ExitSuccess))
import System.Process (readProcessWithExitCode)

import Config

--------------------------------------------------------------------------------

-- Determines how to present git information
data GitCommitContent = Hash | Commit | Full
   deriving (Eq, Read)


-- Field that contains the latest commit hash for the given item
commitField :: String -> GitCommitContent -> Context String
commitField name content = field name $ \item -> unsafeCompiler $ do
  let path = toFilePath $ itemIdentifier item
  getGitCommit content $ sourceDir ++ path


-- Field that contains the commit hash of HEAD
headCommitField :: String -> GitCommitContent -> Context String
headCommitField name content = field name $
  \_ -> unsafeCompiler $ getGitCommit content "."

--------------------------------------------------------------------------------

instance Show GitCommitContent where
  show content = case content of
      Hash -> "%h"
      Commit -> "%h: %s"
      Full -> "%h: %s (%ai)"


getGitCommit :: GitCommitContent -> FilePath -> IO String
getGitCommit content path = do
  (status, stdout, _) <- readProcessWithExitCode "git" args ""
  pure $ case status of
    ExitSuccess -> trim stdout
    _ -> ""
  where trim = dropWhileEnd isSpace
        args = [ "log", "-1", "--format=" ++ show content, "--", path ]
