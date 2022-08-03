{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hakyll
import Hakyll.Images (loadImage, compressJpgCompiler, ensureFitCompiler)

import Config
import Context
import Field
import Page
import Route
import Snippet
import Style
import Thirdparty
import Util

-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = hakyllWith hakyllConfig $ do

  -- Build tags
  tags <- buildTags contentGlob (fromCapture "tag/*/index.html")

  -- Compress JPEG imges
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

  -- Copy third-party content
  copyThirdpartyFilesFrom "THIRDPARTY" "assets/thirdparty/"
    [ ("line-awesome", "line-awesome")
    , ("mathjax", "mathjax")
    , ("revealjs/plugin", "revealjs/plugin")
    , ("revealjs/dist", "revealjs")
    , ("swiperjs", "swiperjs")
    , ("vanillajs-scrollspy", "vanillajs-scrollspy")
    ]

  -- Compile bibliographies
  match "**.bib" $ compile biblioCompiler
  match "**.csl" $ compile cslCompiler

  -- Compile templates
  match "templates/**" $ compile templateBodyCompiler

  -- Create home page
  createHomepageFrom "index.html" tags

  -- Create simple static pages
  createAboutPageFrom "about.md"

  -- Create content archives
  createDateArchive "archive.html" "Archive" contentGlob tags
  createDateArchive "blog.html" "Blog" postsGlob tags
  createDateArchive "projects.html" "Projects" projectsGlob tags
  createProjectArchive "posts-by-project.html" "Posts By Project" tags

  -- Assemble site content
  assembleBlogPosts tags
  assembleProjects tags

  -- Assemble tag page and pages for each tag
  createTagsPage "tags.html" "Tags" tags
  assembleTagPages tags

  -- Create atom and rss feeds
  createFeed Atom "atom.xml"
  createFeed Rss "rss.xml"

  -- Create sitemap
  createSitemap "sitemap.xml" tags

  -- Create CNAME file
  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem domain
