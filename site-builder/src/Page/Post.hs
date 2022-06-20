{-# LANGUAGE OverloadedStrings #-}

module Page.Post
( assembleBlogPosts
) where

import Hakyll
import Control.Monad (filterM, mapM)
import Data.Char (toLower)
import Data.List (filter, findIndex)
import Data.Maybe (fromMaybe, isNothing)

import Config
import Context
import Field
import Route
import Snippet
import Util

--------------------------------------------------------------------------------

assembleBlogPosts :: Tags -> Tags -> Bool -> Rules ()
assembleBlogPosts tags categories isDevelopment =
  matchMetadata (postsGlob .&&. hasNoVersion) (\m -> isDevelopment || lookupString "status" m == Just "published") $ do
    version "simple" $ do
      route $ composeRoutes (cleanRouteContent "blog") idRoute
      compile $ do
        item <- pandocCompilerWithTransform readerOptions defaultHakyllWriterOptions substituteSnippets
        simpleCompile item
    route $ composeRoutes (cleanRouteContent "blog") idRoute
    compile $ do
      item <- pandocCompilerWithTransform readerOptions defaultHakyllWriterOptions substituteSnippets
      ctx <- postContext item tags categories
      buildPost item ctx

--------------------------------------------------------------------------------

buildPost :: Item String -> Context String -> Compiler (Item String)
buildPost item ctx =
  pure item >>= bibliography
            >>= saveSnapshot "posts-content"
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= saveSnapshot "posts-rendered"
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls
            >>= cleanIndexUrls
            >>= localAssetsUrls


postContext :: Item String -> Tags -> Tags -> Compiler (Context String)
postContext item tags categories = do
  m <- getMetadata $ itemIdentifier item
  posts <- recentFirst =<< loadAll (postsGlob .&&. hasVersion "simple")
  ctx <- case lookupString "project" m of
    Nothing -> do
      posts' <- filterM hasNoProject posts
      getNextAndPrev item posts' tags categories
    Just slug -> do
      projects <- recentFirst =<< loadAll (projectsGlob .&&. hasVersion "simple")
      let projects' = filter (isMatchingProject slug) projects
      if null projects'
        then pure mempty
        else do
          posts' <- filterM (isProjectPost slug) posts
          prevNext <- getNextAndPrev item posts' tags categories
          pure $ prevNext <> listField "related-project" (projectContext tags categories) (pure $ take 1 projects')
  pure $ ctx <> blogPostContext tags categories


getNextAndPrev :: Item String -> [Item String] -> Tags -> Tags -> Compiler (Context String)
getNextAndPrev item content tags categories = pure $
  listField "previous-post" (blogPostContext tags categories) (pure previous)
  <> listField "next-post" (blogPostContext tags categories) (pure next)
  where currentIndex = fromMaybe (-2) (findIndex (\i -> getSlug i == getSlug item) content)
        findAt index = [content !! index | index >= 0 && index < length content]
        previous = findAt $ currentIndex + 1
        next = findAt $ currentIndex - 1


isMatchingProject :: String -> Item String -> Bool
isMatchingProject slug item = map toLower slug == projectSlug
  where projectSlug = getSlug item


isProjectPost :: String -> Item String -> Compiler Bool
isProjectPost projectSlug item = do
  m <- getMetadata $ itemIdentifier item
  case lookupString "project" m of
    Nothing -> pure False
    Just slug -> pure $ map toLower slug == map toLower projectSlug


hasNoProject :: Item String -> Compiler Bool
hasNoProject item = do
  m <- getMetadata $ itemIdentifier item
  pure . isNothing $ lookupString "project" m
