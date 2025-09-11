{-# LANGUAGE OverloadedStrings #-}

module Page.Archive.Project
( createProjectArchive
) where

import Hakyll

import Config
import Context
import Field
import Route
import Util

--------------------------------------------------------------------------------

createProjectArchive :: Identifier -> String -> Tags -> Rules ()
createProjectArchive url title tags = create [url] $ do
  route cleanRoute
  compile $ do
    posts <- recentFirst =<< loadAllSnapshots postsGlob "posts-content"
    projects <- recentFirst =<< loadAllSnapshots projectsGlob "posts-content"
    let ctx = groupByProjectField "projects" posts projects contentCtx
           <> constField "title" title
           <> siteContext
    makeItem []
      >>= loadAndApplyTemplate "templates/archive-by-project.html" ctx
      >>= loadAndApplyTemplate "templates/page.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
      >>= cleanIndexUrls
  where contentCtx = contentContext tags

--------------------------------------------------------------------------------
