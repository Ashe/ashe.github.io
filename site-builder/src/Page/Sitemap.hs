{-# LANGUAGE OverloadedStrings #-}

module Page.Sitemap
( createSitemap
) where

import Hakyll

import Config
import Context

--------------------------------------------------------------------------------

createSitemap :: Identifier -> Tags -> Tags -> Rules ()
createSitemap url tags categories = create [url] $ do
  route idRoute
  compile $ do

    -- Load and sort the posts
    posts <- recentFirst =<< loadAll contentGlob

    -- Load individual pages from a list (globs DO NOT work here)
    singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

    -- Mappend the posts and singlePages together
    let pages = posts <> singlePages

        postCtx' t c = dateField "date" "%Y-%m-%d" <> contentContext t c

        -- Create the `pages` field with the postCtx containing standard date
        -- and return the `pages` value for it
        sitemapCtx = constField "root" root 
                  <> listField "pages" (postCtx' tags categories) (pure pages)

    -- Make the item and apply our sitemap template
    makeItem ""
      >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
