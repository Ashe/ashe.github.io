{-# LANGUAGE OverloadedStrings #-}

module Snippet.Section
( toSectionHeader
) where

import Hakyll
import Text.Pandoc.Definition (Block(..), Inline(..))
import qualified Data.Text as T

--------------------------------------------------------------------------------

toSectionHeader :: Block -> Block
toSectionHeader (Header level (id, _, _) content) = newSection
    where newSection = Div (id, ["article-section"], []) [header]
          header = Header level ("", [], []) content' 
          content' = 
            [ Span ("", [], []) content
            , Link ("", ["anchor", "las", "la-link"], [])
                   []
                   (T.pack $ "#"++ T.unpack id, id)
            ]

--------------------------------------------------------------------------------
