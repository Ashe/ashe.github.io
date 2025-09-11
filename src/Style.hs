{-# LANGUAGE OverloadedStrings #-}

module Style
( compileStyleWith
) where

import Hakyll
import Hakyll.Web.Sass (sassCompiler, sassCompilerWith)
import Data.List(stripPrefix)
import Data.Maybe(Maybe(..))
import System.Environment (lookupEnv)
import Text.Sass.Functions (SassImport, SassImporter(..), makePathImport, SassImporterType)
import Text.Sass.Options (SassOptions(..), defaultSassOptions, SassOutputStyle(..))

--------------------------------------------------------------------------------

compileStyleWith :: String -> Rules ()
compileStyleWith includeEnv = do
  scssDependency <- makePatternDependency "assets/css/**.scss"
  rulesExtraDependencies [scssDependency] $ 
    match "assets/css/main.scss" $ do
      route $ setExtension "css"
      compile $ do
        compiler <- unsafeCompiler $ getSassCompiler includeEnv
        fmap compressCss <$> compiler

--------------------------------------------------------------------------------

sassOptions :: [SassImporter] -> SassOptions
sassOptions imports = defaultSassOptions
    { sassSourceMapEmbed  = True
    , sassOutputStyle     = SassStyleCompressed
    , sassImporters       = Just imports
    }


importer :: String ->  SassImporterType
importer thirdparty imp file = pure [makePathImport path]
  where path = case stripPrefix "thirdparty" imp of
          Just end -> thirdparty ++ end
          _ -> imp


getSassCompiler :: String -> IO (Compiler (Item String))
getSassCompiler env = do
  mIncludes <- lookupEnv env
  case mIncludes of
    Just includes -> do
      let options = sassOptions [SassImporter 1 $ importer includes]
      pure $ sassCompilerWith options
    _ -> do
      putStrLn $ "Error: Couldn't find anything for environment variable '" ++ env ++ "'."
      pure sassCompiler
