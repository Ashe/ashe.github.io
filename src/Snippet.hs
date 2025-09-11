module Snippet 
( substituteSnippets
) where

import Hakyll
import Text.Pandoc.Definition (Pandoc (..), Block(..))
import Text.Pandoc.Walk (walk)

import Snippet.Caption
import Snippet.Figure
import Snippet.InfoBox
import Snippet.Section

--------------------------------------------------------------------------------

substituteSnippets :: Pandoc -> Pandoc
substituteSnippets (Pandoc meta blocks) = Pandoc meta sections
  where sections = foldl toSections [] blocks

--------------------------------------------------------------------------------

-- Organise blocks into sections sorted by header
toSections :: [Block] -> Block -> [Block]
toSections sections block = block `insertInto` sections


-- Insert a block into its respective section
insertInto :: Block -> [Block] -> [Block]
insertInto b [] = [substitute b]
insertInto b sections
  | shouldNestInto b sections = init sections ++ [b `nestInto` last sections]
  | otherwise = sections ++ [substitute b]
  where nestInto b s = let (Div a s') = s in Div a (b `insertInto` s')


-- Determine whether to place the block in the current structure or append to it
shouldNestInto :: Block -> [Block] -> Bool
shouldNestInto (Header l _ _) sections = isNewLevel l (last sections)
  where isNewLevel level (Div _ (Header l _ _ : _)) = level > l
        isNewLevel _ _ = False
shouldNestInto block sections = isSection (last sections)
  where isSection (Div _ (Header{} : _)) = True
        isSection _ = False


-- Substitute blocks depending on content
substitute :: Block -> Block
substitute header@Header{} = substituteHeader header
substitute div@Div{} = substituteDiv div
substitute block = block


-- Headers
substituteHeader :: Block -> Block
substituteHeader = toSectionHeader


-- Divs
substituteDiv :: Block -> Block
substituteDiv div@(Div attrs content)
  | isCaption attrs = toCaption div
  | isFigure attrs = toFigure div
  | isInfoBox attrs = toInfoBox div
  | otherwise = div

