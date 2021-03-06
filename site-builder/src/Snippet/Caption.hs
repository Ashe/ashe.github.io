{-# LANGUAGE OverloadedStrings #-}

module Snippet.Caption
( isCaption
, toCaption
) where

import Hakyll
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Text.Pandoc (nullAttr)
import Text.Pandoc.Definition (Attr, Block(..), Inline(..))
import qualified Data.Text as T

import Snippet.Util

--------------------------------------------------------------------------------

isCaption :: Attr -> Bool
isCaption = hasClass "caption"


toCaption :: Block -> Block
toCaption (Div attr@(id, c, _) content) =
  Div (id, c', []) $ content ++ [makeCaption attr]
    where c' = delete "caption" c ++ ["caption-frame apply-shadow"]

--------------------------------------------------------------------------------

makeCaption :: Attr -> Block
makeCaption attr@(id, _, kvp) = case lookup "caption" kvp of
  Nothing -> Null
  Just caption -> Div ("", classes, [])
    (parse caption ++ [Div ("", sourceClasses, []) [source]])
  where classes = ["caption"]
        sourceClasses = ["caption-source"]
        sourceUrl = lookup "sourceUrl" kvp
        source = case lookup "source" kvp of
          Just source -> makeSource source sourceUrl
          Nothing -> Null

makeSource :: T.Text -> Maybe T.Text -> Block
makeSource src Nothing = Plain [ Str src ]
makeSource src (Just url) = Plain [ Link nullAttr [ Str src] (url, src) ]
