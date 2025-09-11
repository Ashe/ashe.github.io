{-# LANGUAGE OverloadedStrings #-}
module Page.Home
( createHomepageFrom
) where

import Hakyll
import Control.Monad (filterM)

import Config
import Context
import Field
import Route
import Util

--------------------------------------------------------------------------------

createHomepageFrom :: Pattern -> Tags -> Rules ()
createHomepageFrom source tags = match source $ do
  route cleanRoute
  compile $ do
    let contentCtx = contentContext tags
        projCtx = projectContext tags
        isFeatured item = do
          m <- getMetadata $ itemIdentifier item
          pure $ lookupString "featured" m == Just "true"
    posts <- recentFirst =<< loadAll (contentGlob .&&. hasNoVersion)
    projects <- recentFirst =<< loadAll (projectsGlob .&&. hasNoVersion)
    featuredProjects <- filterM isFeatured projects
    let indexCtx = listField "posts" contentCtx (pure $ take 16 posts)
                <> listField "projects" projCtx (pure $ take 5 featuredProjects)
                <> constField "item-type" "home"
                <> allTagsCloudField tags
                <> siteContext
    getResourceBody
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/page.html" indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls
      >>= cleanIndexUrls

--------------------------------------------------------------------------------
