module Field.Markdown
( markdownField
) where

import Hakyll
import Data.List (isPrefixOf, isSuffixOf)

import Config

--------------------------------------------------------------------------------

markdownField :: String -> Context String
markdownField name = functionField name $ \args item -> do
  m <- getMetadata $ itemIdentifier item
  case lookupString (concat args) m of
    Nothing -> pure []
    Just str -> do
      md <- makeItem str
      p <- readPandocWith readerOptions md
      pure $ itemBody $ writePandoc p

--------------------------------------------------------------------------------
