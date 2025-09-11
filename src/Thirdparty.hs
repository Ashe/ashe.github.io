{-# LANGUAGE OverloadedStrings #-}

module Thirdparty
( Import (..)
, copyThirdpartyFilesFrom
) where

import Hakyll

import Control.Monad (forM_)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

import qualified Data.ByteString as B

--------------------------------------------------------------------------------

-- Representation of an import scheme
data Import = Import FilePath | ImportTo FilePath FilePath


-- Imports files as directed by import scheme
copyThirdpartyFilesFrom :: String -> FilePath -> [Import] -> Rules ()
copyThirdpartyFilesFrom includeEnv destination mappings = do
  thirdparty <- preprocess $ lookupEnv includeEnv
  case thirdparty of
    Just thirdpartyRoot -> do
      forM_ mappings $ walkFrom thirdpartyRoot destination
    _ -> pure ()

--------------------------------------------------------------------------------

walkFrom :: FilePath -> FilePath -> Import -> Rules ()
walkFrom root dest (Import path) = walkPaths (root </> path) (dest </> path) ""
walkFrom root dest (ImportTo from to) = walkPaths (root </> from) (dest </> to) ""


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
