{-# LANGUAGE OverloadedStrings #-}

module Snippet.Figure
( isFigure
, toFigure
) where

import Hakyll
import Data.List (delete)
import Data.Maybe (fromMaybe)
import Text.Pandoc (nullAttr)
import Text.Pandoc.Definition (Attr, Block(..), Inline(..))
import qualified Data.Text as T

import Snippet.Caption
import Snippet.Util

--------------------------------------------------------------------------------

isFigure :: Attr -> Bool
isFigure = hasClass "figure"


toFigure :: Block -> Block
toFigure (Div attr@(id, c, kvp) content) = case lookup "image" kvp of
  Nothing -> Null
  Just imageSrc -> toCaption $ Div ("", delete "figure" c, kvp) 
    [ Plain [makeImage imageSrc kvp]
    ]

--------------------------------------------------------------------------------

makeImage :: T.Text -> [(T.Text, T.Text)] -> Inline
makeImage imageSrc kvp = Image ("", classes, []) [ Str alt ] (imageSrc, title)
  where classes = ["w-full", "object-cover"]
        caption = fromMaybe "" $ lookup "caption" kvp
        alt = fromMaybe caption $ lookup "image-alt" kvp
        title = fromMaybe alt $ lookup "image-title" kvp
