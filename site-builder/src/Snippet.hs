module Snippet 
( substituteSnippets
) where

import Hakyll
import Text.Pandoc.Definition (Pandoc (..), Block(..))
import Text.Pandoc.Walk (walk)

import Snippet.Anchor
import Snippet.Caption
import Snippet.Figure
import Snippet.InfoBox

--------------------------------------------------------------------------------

substituteSnippets :: Pandoc -> Pandoc
substituteSnippets = walk transform 
  where transform header@Header{} = transformHeader header
        transform div@Div{} = transformDiv div
        transform block = block

--------------------------------------------------------------------------------

-- Headers
transformHeader :: Block -> Block
transformHeader = toHeaderAnchor

-- Divs
transformDiv :: Block -> Block
transformDiv div@(Div attrs content)
  | isCaption attrs = toCaption div
  | isFigure attrs = toFigure div
  | isInfoBox attrs = toInfoBox div
  | otherwise = div
