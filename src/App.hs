module App
  ( runCommand
  , createGitkeep
  , shouldCreateGitkeep
  , isIgnoredByGit
  , isInsideGitDir
  , isGitFolder
  , gitCheckIgnore
  , createHere
  , traverseChildren
  , isRoot
  ) where

import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , canonicalizePath
  )
import System.Directory.Extra (listDirectories)
import System.FilePath
  ( takeDirectory
  , (</>)
  )
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Paths_git_keep (version)
import Data.Version (showVersion)

runCommand :: [String] -> IO ()
runCommand args =
  case args of
    ["-h"] -> putStrLn usage
    ["-v"] -> putStrLn (showVersion version)
    ["-r"] -> runRecursive
    []     -> runNonRecursive
    _      -> putStrLn usage
  where
    runRecursive    = createGitkeep True "."
    runNonRecursive = createGitkeep False "."
    usage =
      "Usage: git keep [-r]\n\
      \  -r  create .gitkeep files recursively"

createGitkeep :: Bool -> FilePath -> IO ()
createGitkeep recursive path = do
  absPath <- canonicalizePath path
  shouldProcess <- shouldCreateGitkeep absPath
  if shouldProcess
    then createHere absPath
    else pure ()

  if recursive
    then traverseChildren absPath
    else pure ()

shouldCreateGitkeep :: FilePath -> IO Bool
shouldCreateGitkeep path = do
  isDir <- doesDirectoryExist path
  ignored <- isIgnoredByGit path
  pure (isDir && not ignored)

isIgnoredByGit :: FilePath -> IO Bool
isIgnoredByGit path = do
  insideGitDir <- isInsideGitDir path
  if insideGitDir
    then pure True
    else gitCheckIgnore path

isInsideGitDir :: FilePath -> IO Bool
isInsideGitDir path = go path
  where
    go dir
      | isRoot dir = pure False
      | otherwise = do
          isGit <- isGitFolder dir
          if isGit
            then pure True
            else go (takeDirectory dir)

isGitFolder :: FilePath -> IO Bool
isGitFolder path = do
  let gitDir  = path </> ".git"
  let gitFile = path </> ".git"
  isDir  <- doesDirectoryExist gitDir
  isFile <- doesFileExist gitFile
  pure (isDir || isFile)

gitCheckIgnore :: FilePath -> IO Bool
gitCheckIgnore path = do
  (code, _, _) <- readProcessWithExitCode
                    "git"
                    ["check-ignore", "-q", path]
                    ""
  pure (code == ExitSuccess)

createHere :: FilePath -> IO ()
createHere path = do
  let gitkeepPath = path </> ".gitkeep"
  exists <- doesFileExist gitkeepPath
  if exists
    then pure ()
    else do
      writeFile gitkeepPath ""
      putStrLn ("Created .gitkeep at " ++ gitkeepPath)

traverseChildren :: FilePath -> IO ()
traverseChildren path = do
  dirs <- listDirectories path
  mapM_ (createGitkeep True) dirs

isRoot :: FilePath -> Bool
isRoot p = takeDirectory p == p
