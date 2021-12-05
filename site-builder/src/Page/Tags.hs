{-# LANGUAGE OverloadedStrings #-}

module Page.Tags
( createTagsPage
) where

import Hakyll

import Config
import Context
import Field
import Route
import Util

--------------------------------------------------------------------------------

createTagsPage :: Identifier -> String -> Tags -> Rules ()
createTagsPage url title tags = create [url] $ do
  route cleanRoute
  compile $ do
    let tagsCtx = constField "title" title
               <> tagCloudField "tag-cloud" 110 550 (randomiseTags tags)
               <> siteContext
    makeItem []
      >>= applyAsTemplate siteContext
      >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
      >>= loadAndApplyTemplate "templates/page.html" tagsCtx
      >>= loadAndApplyTemplate "templates/default.html" tagsCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

--------------------------------------------------------------------------------
