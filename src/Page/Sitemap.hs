{-# LANGUAGE OverloadedStrings #-}

module Page.Sitemap
( createSitemap
) where

import Hakyll

import Config
import Context

--------------------------------------------------------------------------------

createSitemap :: Identifier -> Tags -> Rules ()
createSitemap url tags = create [url] $ do
  route idRoute
  compile $ do

    -- Load and sort the posts
    posts <- recentFirst =<< loadAll contentGlob

    -- Load individual pages from a list (globs DO NOT work here)
    singlePages <- loadAll (fromList ["about.rst", "contact.markdown"])

    -- Mappend the posts and singlePages together
    let pages = posts <> singlePages

        postCtx' t = dateField "date" "%Y-%m-%d" <> contentContext t

        -- Create the `pages` field with the postCtx containing standard date
        -- and return the `pages` value for it
        sitemapCtx = constField "root" root 
                  <> listField "pages" (postCtx' tags) (pure pages)

    -- Make the item and apply our sitemap template
    makeItem ""
      >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
