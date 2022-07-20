{-# LANGUAGE OverloadedStrings #-}

module Field.Bibliography
( bibliography
) where

import Hakyll
import System.Directory (doesFileExist)
import System.FilePath.Posix (dropExtension, isExtensionOf, (</>))
import Text.Pandoc (nullAttr)
import Text.Pandoc.Definition (Block(..), Inline(..))
import Text.Pandoc.Walk (walk)


import Config
import Snippet.Section
import Snippet.InfoBox

--------------------------------------------------------------------------------

-- Check if there's a bibliography file for this item and build its references
bibliography :: Item String -> Compiler (Item String)
bibliography item = do

  let bibFile = dropExtension (toFilePath $ itemIdentifier item) ++ ".bib"
      bibFilePath = sourceDir </> bibFile
  hasBibFile <- unsafeCompiler $ doesFileExist bibFilePath
  if not hasBibFile
    then pure item
    else do
      bib <- load $ fromFilePath bibFile
      csl <- load $ fromFilePath "assets/ieee.csl"
      pandoc <- readPandocBiblio readerOptions csl bib item

      pure $ writePandoc $ walk makeBibliographyBlock pandoc
      where
        replaceElements refs@(Div ("refs", _, _) _) =
          Div ("references", ["box", "fill-horizontal"] , []) [header, refs]
        replaceElements block = block
        header = Div ("", ["header"], [])
          [ Para
            [ Span ("", ["las", "la-book", "mr-3"],[]) []
            , Str "References"
            ]
          ]

--------------------------------------------------------------------------------

makeBibliographyBlock :: Block -> Block
makeBibliographyBlock refs@(Div ("refs", _, _) _) = refSection
  where header = Header 1 ("references", [], []) [Str "References"]
        (Div attr blocks) = toSectionHeader header
        refSection = Div attr (blocks ++ [makeRefBox refs])
makeBibliographyBlock block = block


makeRefBox :: Block -> Block
makeRefBox (Div (_, classes, kvp) refs) = toInfoBox refbox
  where refbox = Div ("", classes, kvp') refs
        kvp' = kvp ++ [("header", "References")]