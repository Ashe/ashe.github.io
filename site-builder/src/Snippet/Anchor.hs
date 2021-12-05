{-# LANGUAGE OverloadedStrings #-}

module Snippet.Anchor
( toHeaderAnchor
) where

import Hakyll
import Text.Pandoc.Definition (Block(..), Inline(..))
import qualified Data.Text as T

--------------------------------------------------------------------------------

-- Headers: Add anchor links to each header to link to parts of the post
toHeaderAnchor :: Block -> Block
toHeaderAnchor (Header level (id, classes, kvp) content) = 
  Header level (id, classes', kvp) content' 
    where classes' = classes ++ ["anchor-header"]
          content' = 
            [ Span ("", [], []) content
            , Link ("", ["anchor", "las", "la-link"], [])
                   []
                   (T.pack $ "#"++ T.unpack id, id)
            ]

--------------------------------------------------------------------------------
