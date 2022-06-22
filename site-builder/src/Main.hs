{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll
import Data.List (isPrefixOf, isSuffixOf)
import Hakyll.Images (loadImage, compressJpgCompiler, ensureFitCompiler)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath.Posix (takeFileName)
import System.Process (readProcessWithExitCode)

import Config
import Context
import Field
import Page
import Route
import Snippet
import Style
import Util

-- Configuration
--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration 
  { destinationDirectory = "build/site"
  , storeDirectory       = "build/_store"
  , tmpDirectory         = "build/_tmp"
  , providerDirectory    = sourceDir
  , ignoreFile           = ignoreFile'
  } 
  where ignoreFile' path
         | "."`isPrefixOf` fileName = True
         | otherwise = False
         where fileName = takeFileName path

-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith config $ do

  -- Build tags and categories
  tags <- buildTags contentGlob (fromCapture "tag/*/index.html")
  categories <- buildCategories contentGlob (fromCapture "posts/*.html")
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    let ctx   = contentContext tags categories
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
      let tagsCtx = constField "title" title
                 <> listField "posts" ctx (pure posts)
                 <> constField "tag" tag
                 <> tagCloudField "tag-cloud" 110 550 (randomiseTags tags)
                 <> siteContext
      makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
          >>= loadAndApplyTemplate "templates/page.html" tagsCtx
          >>= loadAndApplyTemplate "templates/default.html" tagsCtx
          >>= relativizeUrls
          >>= cleanIndexUrls

    -- Compress JPEG images
    match jpgs $ version "large" $ do
      route idRoute
      compile $ loadImage
        >>= compressJpgCompiler 50

    -- Create small versions of JPEGs
    match jpgs $ version "small" $ do
      route $ fileSuffixRoute "small"
      compile $ loadImage
        >>= ensureFitCompiler 1200 600
        >>= compressJpgCompiler 90

    -- Copy PNG images, icons, favicons etc
    match ("assets/images/**" .&&. complement jpgs) $ do
      route idRoute
      compile copyFileCompiler

    -- Copy scripts
    match "assets/scripts/**.js*" $ do
      route idRoute
      compile copyFileCompiler

    -- Copy plain CSS
    match "assets/css/**.css" $ do
      route idRoute
      compile copyFileCompiler

    -- Compile SASS into CSS
    compileStyleWith "THIRDPARTY"

    -- Compile bibliographies
    match "**.bib" $ compile biblioCompiler
    match "**.csl" $ compile cslCompiler

    -- Compile templates
    match "templates/**" $ compile templateBodyCompiler

    -- Create home page
    createHomepageFrom "index.html" tags categories

    -- Create simple static pages
    createAboutPageFrom "about.md"

    -- Create content archives
    createDateArchive "archive.html" "Archive" contentGlob tags categories
    createDateArchive "blog.html" "Blog" postsGlob tags categories
    createDateArchive "projects.html" "Projects" projectsGlob tags categories
    createProjectArchive "posts-by-project.html" "Posts By Project" tags categories

    -- Assemble site content
    assembleBlogPosts tags categories
    assembleProjects tags categories

    -- Assemble tag page
    createTagsPage "tags.html" "Tags" tags

    -- Create atom and rss feeds
    createFeed Atom "atom.xml"
    createFeed Rss "rss.xml"

    -- Create sitemap
    createSitemap "sitemap.xml" tags categories

    -- Create CNAME file
    create ["CNAME"] $ do
      route idRoute
      compile $ makeItem domain
