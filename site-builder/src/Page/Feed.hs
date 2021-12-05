{-# LANGUAGE OverloadedStrings #-}

module Page.Feed 
( FeedType (..)
, createFeed
) where

import Hakyll

import Config
import Context

--------------------------------------------------------------------------------

data FeedType = Atom | Rss

createFeed :: FeedType -> Identifier -> Rules ()
createFeed feedType url = create [url] $ do
  route idRoute
  compile $ feedCompiler (renderFunction feedType)

--------------------------------------------------------------------------------

type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)

renderFunction :: FeedType -> FeedRenderer
renderFunction Atom = renderAtom
renderFunction Rss = renderRss

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = domain
    , feedDescription = siteDescription
    , feedAuthorName  = socialName
    , feedAuthorEmail = socialEmail
    , feedRoot        = root
    }

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedContext
        =<< fmap (take 10 ) . recentFirst
        =<< loadAllSnapshots contentGlob "posts-content"

