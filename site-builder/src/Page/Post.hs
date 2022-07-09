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

assembleBlogPosts :: Tags -> Rules ()
assembleBlogPosts tags =
  matchMetadata (postsGlob .&&. hasNoVersion) (\m -> lookupString "status" m == Just "published") $ do
    version "simple" $ do
      route $ composeRoutes (cleanRouteContent "blog") idRoute
      compile simpleCompile
    route $ composeRoutes (cleanRouteContent "blog") idRoute
    compile $ do
      item <- pandocCompilerWithTransform readerOptions defaultHakyllWriterOptions substituteSnippets
      ctx <- postContext item tags
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


postContext :: Item String -> Tags -> Compiler (Context String)
postContext item tags = do
  m <- getMetadata $ itemIdentifier item
  posts <- recentFirst =<< loadAll (postsGlob .&&. hasVersion "simple")
  nextAndPrev <- getNextAndPrev item posts tags
  relatedProject <- case lookupString "project" m of
    Just slug -> do
      projects <- recentFirst =<< loadAll (projectsGlob .&&. hasVersion "simple")
      let projects' = filter (isMatchingProject slug) projects
      if not $ null projects'
        then pure $ 
          listField "related-project" (projectContext tags) (pure $ take 1 projects')
        else pure mempty
    _ -> pure mempty
  pure $ nextAndPrev <> relatedProject <> blogPostContext tags


getNextAndPrev :: Item String -> [Item String] -> Tags -> Compiler (Context String)
getNextAndPrev item content tags = pure $
  listField "previous-post" (blogPostContext tags) (pure previous)
  <> listField "next-post" (blogPostContext tags) (pure next)
  where currentIndex = fromMaybe (-2) (findIndex (\i -> getSlug i == getSlug item) content)
        findAt index = [content !! index | index >= 0 && index < length content]
        previous = findAt $ currentIndex + 1
        next = findAt $ currentIndex - 1


isMatchingProject :: String -> Item String -> Bool
isMatchingProject slug item = map toLower slug == projectSlug
  where projectSlug = getSlug item
