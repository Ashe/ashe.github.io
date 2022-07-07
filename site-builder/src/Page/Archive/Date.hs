{-# LANGUAGE OverloadedStrings #-}

module Page.Archive.Date
( createDateArchive
) where

import Hakyll

import Context
import Field
import Route
import Util

--------------------------------------------------------------------------------

createDateArchive :: Identifier -> String -> Pattern -> Tags -> Rules ()
createDateArchive url title glob tags = create [url] $ do
  route cleanRoute
  compile $ do
    content <- recentFirst =<< loadAll (glob .&&. hasVersion "simple")
    let ctx = groupByYearField "years" content contentCtx
           <> constField "title" title
           <> siteContext
    makeItem []
      >>= loadAndApplyTemplate "templates/archive-by-date.html" ctx
      >>= loadAndApplyTemplate "templates/page.html" ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls
      >>= cleanIndexUrls
  where contentCtx = contentContext tags

--------------------------------------------------------------------------------
