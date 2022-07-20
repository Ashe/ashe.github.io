{-# LANGUAGE OverloadedStrings #-}

module Config
( sourceDir
, domain
, root
, hakyllConfig
, siteRepo
, siteDescription
, siteLogo
, socialName
, socialProfilePicture
, socialEmail
, socialGithub
, socialLinkedin
, socialInstagram
, postsGlob
, projectsGlob
, contentGlob
, jpgs
, svg
, readerOptions
) where

import Hakyll
import Data.List (isPrefixOf)
import System.FilePath.Posix (takeFileName)
import Text.Pandoc (ReaderOptions (readerExtensions), def, extensionsFromList, Extensions)
import Text.Pandoc.Options (Extension(..))

-- Site configuration
--------------------------------------------------------------------------------

sourceDir :: String
sourceDir = "data/"

domain :: String
domain = "aas.sh"

root :: String
root = "https://" ++ domain

-- Hakyll Config
--------------------------------------------------------------------------------

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration
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

-- Site details
--------------------------------------------------------------------------------

siteRepo :: String
siteRepo = "https://github.com/ashe/ashe.github.io"

siteDescription :: String
siteDescription = "Hey there! My name is Ashley Smith and I'm a software engineer from the UK. I love working on challenging projects and trying new things — I try to never stop learning."

siteLogo :: String
siteLogo = "/assets/images/logo.png"

-- Socials
--------------------------------------------------------------------------------

socialName :: String
socialName = "Ashley Smith"

socialProfilePicture :: String
socialProfilePicture = "https://res.cloudinary.com/aas-sh/image/upload/v1623408029/site/profile_square.jpg"

socialEmail :: String
socialEmail = "contact@aas.sh"

socialGithub :: String
socialGithub = "ashe"

socialLinkedin :: String
socialLinkedin = "itsashe"

socialInstagram :: String
socialInstagram = "theofficialashe"

-- Patterns
--------------------------------------------------------------------------------

postsGlob :: Pattern
postsGlob = "content/blog/**.md"

projectsGlob :: Pattern
projectsGlob = "content/projects/**.md"

contentGlob :: Pattern
contentGlob = postsGlob .||. projectsGlob

jpgs :: Pattern
jpgs = "**.jpg" .||. "**.jpeg"

svg :: Pattern
svg = "**.svg"

-- Configuration
--------------------------------------------------------------------------------

readerOptions:: ReaderOptions
readerOptions = def {
  readerExtensions = siteReaderExtensions
}


siteReaderExtensions :: Extensions
siteReaderExtensions = extensionsFromList
    -- Site extensions
  [ Ext_citations
  , Ext_emoji
  , Ext_latex_macros
  , Ext_tex_math_dollars
  , Ext_tex_math_double_backslash

  -- Default hakyll reader extensions
  , Ext_smart

  -- Pandoc extensions
  , Ext_footnotes
  , Ext_inline_notes
  , Ext_pandoc_title_block
  , Ext_yaml_metadata_block
  , Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_pipe_tables
  , Ext_citations
  , Ext_raw_tex
  , Ext_raw_html
  , Ext_tex_math_dollars
  , Ext_latex_macros
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  , Ext_inline_code_attributes
  , Ext_raw_attribute
  , Ext_markdown_in_html_blocks
  , Ext_native_divs
  , Ext_fenced_divs
  , Ext_native_spans
  , Ext_bracketed_spans
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_space_in_atx_header
  , Ext_strikeout
  , Ext_superscript
  , Ext_subscript
  , Ext_task_lists
  , Ext_auto_identifiers
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_implicit_header_references
  , Ext_line_blocks
  , Ext_shortcut_reference_links
  , Ext_smart
  ]