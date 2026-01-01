module Main where

import App
import Test.Tasty
import Test.Tasty.HUnit

import System.IO.Temp (withSystemTempDirectory)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesFileExist
  , doesDirectoryExist
  , listDirectory
  , removeFile
  , canonicalizePath
  , removeDirectoryRecursive
  )
import System.FilePath
  ( (</>)
  , takeDirectory
  )
import Control.Monad (when, forM_)
import Prelude hiding (writeFile)
import System.IO (writeFile, readFile)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "App tests"
  [ isRootTests
  , isGitFolderTests
  , isInsideGitDirTests
  , shouldCreateGitkeepTests
  , createHereTests
  , createGitkeepAndTraverseTests
  , gitCheckIgnoreTests
  , isIgnoredByGitTests
  ]

isRootTests :: TestTree
isRootTests = testGroup "isRoot"
  [ testCase "isRoot matches takeDirectory equality" $ withSystemTempDirectory "iskroot" $ \dir -> do
      -- isRoot should be equivalent to (takeDirectory p == p)
      let expected = takeDirectory dir == dir
      isRoot dir @?= expected
  ]

isGitFolderTests :: TestTree
isGitFolderTests = testGroup "isGitFolder"
  [ testCase "isGitFolder true when .git directory exists" $ withSystemTempDirectory "gitdir" $ \dir -> do
      let gitDir = dir </> ".git"
      createDirectory gitDir
      res <- isGitFolder dir
      res @?= True

  , testCase "isGitFolder true when .git file exists" $ withSystemTempDirectory "gitfile" $ \dir -> do
      let gitFile = dir </> ".git"
      writeFile gitFile ""
      res <- isGitFolder dir
      res @?= True

  , testCase "isGitFolder false when no .git present" $ withSystemTempDirectory "nogit" $ \dir -> do
      res <- isGitFolder dir
      res @?= False
  ]

isInsideGitDirTests :: TestTree
isInsideGitDirTests = testGroup "isInsideGitDir"
  [ testCase "isInsideGitDir true for subdirectory of repo with .git" $ withSystemTempDirectory "insidegit" $ \dir -> do
      -- repo/.git and repo/sub/child
      let repo = dir </> "repo"
      let gitDir = repo </> ".git"
      let subdir = repo </> "sub" </> "child"
      createDirectoryIfMissing True (takeDirectory subdir)
      createDirectory gitDir
      createDirectoryIfMissing True subdir
      res <- isInsideGitDir subdir
      res @?= True

  , testCase "isInsideGitDir false when no .git above" $ withSystemTempDirectory "notinsidegit" $ \dir -> do
      let a = dir </> "a"
      createDirectory a
      res <- isInsideGitDir a
      res @?= False
  ]

shouldCreateGitkeepTests :: TestTree
shouldCreateGitkeepTests = testGroup "shouldCreateGitkeep"
  [ testCase "shouldCreateGitkeep true for directory not ignored" $ withSystemTempDirectory "shouldcreate" $ \dir -> do
      -- A plain temporary directory should be a candidate
      res <- shouldCreateGitkeep dir
      res @?= True

  , testCase "shouldCreateGitkeep false for a file path" $ withSystemTempDirectory "shouldcreate-file" $ \dir -> do
      let f = dir </> "afile"
      writeFile f "content"
      res <- shouldCreateGitkeep f
      res @?= False
  ]

createHereTests :: TestTree
createHereTests = testGroup "createHere"
  [ testCase "createHere creates .gitkeep when missing" $ withSystemTempDirectory "createhere" $ \dir -> do
      let gk = dir </> ".gitkeep"
      existsBefore <- doesFileExist gk
      existsBefore @?= False
      createHere dir
      existsAfter <- doesFileExist gk
      existsAfter @?= True
      contents <- readFile gk
      contents @?= ""
  ]

createGitkeepAndTraverseTests :: TestTree
createGitkeepAndTraverseTests = testGroup "createGitkeep and traverseChildren"
  [ testCase "createGitkeep True creates .gitkeep in root and children" $ withSystemTempDirectory "creategitkeep" $ \dir -> do
      -- Setup children
      let child1 = dir </> "child1"
      let child2 = dir </> "child2"
      createDirectory child1
      createDirectory child2

      -- Run recursive creation on the root
      createGitkeep True dir

      -- .gitkeep should be present in root and both children
      doesFileExist (dir </> ".gitkeep") >>= (@?= True)
      doesFileExist (child1 </> ".gitkeep") >>= (@?= True)
      doesFileExist (child2 </> ".gitkeep") >>= (@?= True)

  , testCase "traverseChildren calls createGitkeep for each child" $ withSystemTempDirectory "traverse" $ \dir -> do
      let c1 = dir </> "a"
      let c2 = dir </> "b"
      createDirectory c1
      createDirectory c2
      
      traverseChildren dir
      doesFileExist (c1 </> ".gitkeep") >>= (@?= True)
      doesFileExist (c2 </> ".gitkeep") >>= (@?= True)
  ]

gitCheckIgnoreTests :: TestTree
gitCheckIgnoreTests = testGroup "gitCheckIgnore"
  [ testCase "gitCheckIgnore returns False outside of git repo" $ withSystemTempDirectory "gitcheck" $ \dir -> do
      res <- gitCheckIgnore (dir </> "somefile")
      res @?= False
  ]

isIgnoredByGitTests :: TestTree
isIgnoredByGitTests = testGroup "isIgnoredByGit"
  [ testCase "isIgnoredByGit true when inside a .git folder tree" $ withSystemTempDirectory "ignored" $ \dir -> do
      let repo = dir </> "repo"
      let gitDir = repo </> ".git"
      let sub = gitDir </> "subdir"
      createDirectory repo
      createDirectory gitDir
      createDirectory sub
      res <- isIgnoredByGit sub
      res @?= True

  , testCase "isIgnoredByGit delegates to gitCheckIgnore when not inside .git" $ withSystemTempDirectory "ignored-delegate" $ \dir -> do
      let p = dir </> "file"
      res <- isIgnoredByGit p
      res @?= False
  ]
