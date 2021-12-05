{-# LANGUAGE OverloadedStrings #-}

module Page.Project
( assembleProjects
) where

import Hakyll
import Control.Monad (filterM)
import Data.Char (toLower)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

import Config
import Context
import Field
import Route
import Snippet
import Util

--------------------------------------------------------------------------------

assembleProjects :: Tags -> Tags -> Bool -> Rules ()
assembleProjects tags categories isDevelopment =
  matchMetadata (projectsGlob .&&. hasNoVersion) (\m -> isDevelopment || lookupString "status" m == Just "published") $ do
    version "simple" $ do
      route $ composeRoutes (cleanRouteContent "project") idRoute
      compile $ do
        item <- pandocCompilerWithTransform readerOptions defaultHakyllWriterOptions substituteSnippets
        buildProject item $ projectContext tags categories
    route $ composeRoutes (cleanRouteContent "project") idRoute
    compile $ do
      item <- pandocCompilerWithTransform readerOptions defaultHakyllWriterOptions substituteSnippets
      ctx <- projectPostContext item tags categories
      buildProject item ctx

--------------------------------------------------------------------------------

buildProject :: Item String -> Context String -> Compiler (Item String)
buildProject item ctx = 
  pure item >>= bibliography
            >>= saveSnapshot "posts-content"
            >>= loadAndApplyTemplate "templates/project.html" ctx
            >>= saveSnapshot "posts-rendered"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            >>= cleanIndexUrls
            >>= localAssetsUrls


projectPostContext :: Item String -> Tags -> Tags -> Compiler (Context String)
projectPostContext item tags categories = do 
  posts <- filterM (isProjectPost item) =<< recentFirst =<< loadAll (postsGlob .&&. hasVersion "simple")
  projects <- recentFirst =<< loadAll (projectsGlob .&&. hasVersion "simple")
  nextAndPrev <- getNextAndPrev item projects tags categories
  pure $ (if null posts 
      then mempty 
      else listField "project-posts" (blogPostContext tags categories) (pure posts))
    <> nextAndPrev
    <> projectContext tags categories


isProjectPost :: Item String -> Item String -> Compiler Bool
isProjectPost project item = do
  m <- getMetadata $ itemIdentifier item
  case lookupString "project" m of
    Nothing -> pure False
    Just slug -> pure $ map toLower slug == projectSlug
  where projectSlug = getSlug project


getNextAndPrev :: Item String -> [Item String] -> Tags -> Tags -> Compiler (Context String)
getNextAndPrev item content tags categories = pure $ 
  listField "previous-post" (blogPostContext tags categories) (pure previous)
  <> listField "next-post" (blogPostContext tags categories) (pure next)
  where currentIndex = fromMaybe (-2) (findIndex (\i -> getSlug i == getSlug item) content)
        findAt index = [content !! index | index >= 0 && index < length content]
        previous = findAt $ currentIndex + 1
        next = findAt $ currentIndex - 1
