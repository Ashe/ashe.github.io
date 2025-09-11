{-# LANGUAGE OverloadedStrings #-}

module Page.Tag
( assembleTagPages
) where

import Hakyll

import Config
import Context
import Field
import Route
import Util

--------------------------------------------------------------------------------

assembleTagPages :: Tags -> Rules ()
assembleTagPages tags = tagsRules tags $ \tag pattern -> do
  let title = "Posts tagged \"" ++ tag ++ "\""
  let ctx   = contentContext tags
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
    let tagsCtx = constField "title" title
               <> listField "posts" ctx (pure posts)
               <> constField "tag" tag
               <> allTagsCloudField tags
               <> siteContext
    makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" tagsCtx
        >>= loadAndApplyTemplate "templates/page.html" tagsCtx
        >>= loadAndApplyTemplate "templates/default.html" tagsCtx
        >>= relativizeUrls
        >>= cleanIndexUrls

--------------------------------------------------------------------------------
