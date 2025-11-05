module Main where

import System.Directory (doesDirectoryExist, canonicalizePath)
import System.Directory.Extra (listDirectories)
import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, dropTrailingPathSeparator)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help", "-h"] -> putStrLn "Usage: git keep [-r/--recursive] (use -r/--recursive to create .gitkeep files in all subdirectories)"
    ["--version", "-v"] -> putStrLn "git-keep version 0.1.0.1"
    _ -> do
      let isRecursive = any (`elem` ["--recursive", "-r"]) args
      createGitkeep isRecursive "."

-- Create .gitkeep file(s)
createGitkeep :: Bool -> FilePath -> IO ()
createGitkeep isRecursive path = do
  absPath <- canonicalizePath path
  isDirectory <- doesDirectoryExist absPath
  isIgnored <- isIgnoredByGit absPath
  if not isDirectory || isIgnored
    then return ()
    else do
      let gitkeepPath = absPath ++ "/.gitkeep"
      appendFile gitkeepPath ""

  if isRecursive
    then do
      dirs <- listDirectories absPath
      mapM_ (createGitkeep True) dirs
    else return ()

-- Check if a path is ignored by git
isIgnoredByGit :: FilePath -> IO Bool
isIgnoredByGit path = do
  if getLastPathComponent path == ".git" -- .git folders are ignored by default but `git check-ignore` will still return Fail
    then return True
    else do
      (exitCode, _, _) <- readProcessWithExitCode "git" ["check-ignore", "-q", path] ""
      return (exitCode == ExitSuccess)

getLastPathComponent :: FilePath -> String
getLastPathComponent path = takeFileName (dropTrailingPathSeparator path)