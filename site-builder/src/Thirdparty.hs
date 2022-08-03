{-# LANGUAGE OverloadedStrings #-}

module Thirdparty
( copyThirdpartyFilesFrom
) where

import Hakyll

import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified Data.ByteString as B

--------------------------------------------------------------------------------

copyThirdpartyFilesFrom :: String -> FilePath -> [(FilePath, FilePath)] -> Rules ()
copyThirdpartyFilesFrom includeEnv destination mappings = do
  thirdparty <- preprocess $ lookupEnv includeEnv
  case thirdparty of
    Just thirdpartyRoot -> do
      forM_ mappings $ copyFilesInDir thirdpartyRoot destination
    _ -> pure ()

--------------------------------------------------------------------------------

copyFilesInDir :: FilePath -> FilePath -> (FilePath, FilePath) -> Rules ()
copyFilesInDir root dest (from, to) = walkPaths (root </> from) (dest </> to) ""


walkPaths :: FilePath -> FilePath -> FilePath -> Rules ()
walkPaths from to path = do
  let current = from </> path
  isFile <- preprocess $ doesFileExist current
  isDir <- preprocess $ doesDirectoryExist current
  if isFile then
    copyFile from to path
  else if isDir then do
    files <- preprocess $ listDirectory current
    forM_ files $ walkPaths current (to </> path)
  else
    pure ()


copyFile :: FilePath -> FilePath -> FilePath -> Rules ()
copyFile from to file = create [fromFilePath $ to </> file] $ do
  route idRoute
  compile $ do
    content <- unsafeCompiler $ B.readFile (from </> file)
    makeItem content
