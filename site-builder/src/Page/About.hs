{-# LANGUAGE OverloadedStrings #-}

module Page.About
( createAboutPageFrom
) where

import Hakyll

import Context
import Route
import Util

--------------------------------------------------------------------------------

createAboutPageFrom :: Pattern -> Rules ()
createAboutPageFrom source = match source $ do
  route cleanRoute
  compile $ pandocCompiler
    >>= applyAsTemplate siteContext
    >>= loadAndApplyTemplate "templates/about.html" siteContext
    >>= loadAndApplyTemplate "templates/page.html" siteContext
    >>= loadAndApplyTemplate "templates/default.html" siteContext
    >>= relativizeUrls
    >>= cleanIndexUrls

--------------------------------------------------------------------------------
