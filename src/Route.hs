module Route
( cleanRoute
, cleanRouteContent
, fileSuffixRoute
) where

import Hakyll
import Data.Char (toLower)
import System.FilePath.Posix (takeBaseName, takeDirectory, takeExtension, (</>))

-- Adapted from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where createIndexRoute ident = toLower <$> index
          where p = toFilePath ident
                baseName = takeBaseName p
                index = if baseName == "index"
                        then "index.html"
                        else baseName </> "index.html"

cleanRouteContent :: FilePath -> Routes
cleanRouteContent location = customRoute createIndexRoute
  where createIndexRoute ident = location </> (toLower <$> index)
          where p = toFilePath ident
                baseName = takeBaseName p
                index = if baseName == "index"
                        then "index.html"
                        else baseName </> "index.html"

fileSuffixRoute :: String -> Routes
fileSuffixRoute suffix = customRoute makeSuffixRoute
  where makeSuffixRoute ident = parentDir </> suffixed
          where
            p = toFilePath ident
            parentDir = takeDirectory p
            baseName = takeBaseName p
            ext = takeExtension p
            suffixed = baseName ++ "-" ++ suffix ++ ext
