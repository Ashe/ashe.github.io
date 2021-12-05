{-# LANGUAGE OverloadedStrings #-}

module Field.Bibliography
( bibliography
) where

import Hakyll
import System.FilePath.Posix (dropExtension, isExtensionOf, (</>))
import Text.Pandoc (nullAttr)
import Text.Pandoc.Definition (Block(..), Inline(..))
import Text.Pandoc.Walk (walk)

import Config

--------------------------------------------------------------------------------

-- Check if there's a bibliography file for this item and build its references
bibliography :: Item String -> Compiler (Item String)
bibliography item = do

  let itemDir = dropExtension (toFilePath $ itemIdentifier item) ++ "/"
  let searchdir = sourceDir </> itemDir
  contents <- unsafeCompiler $ getRecursiveContents (pure . not . isExtensionOf ".bib") searchdir
  if null contents
    then pure item
    else do
      let bibFile = itemDir </> head contents
      unsafeCompiler $ putStrLn $ "  compiling references from: '" ++ bibFile ++ "'"
      bib <- load $ fromFilePath bibFile
      csl <- load $ fromFilePath "assets/ieee.csl"
      pandoc <- readPandocBiblio readerOptions csl bib item

      pure $ writePandoc $ walk replaceElements pandoc 
      where
        replaceElements refs@(Div ("refs", _, _) _) = 
          Div ("", ["box", "fill-horizontal", "bg-muted dark:bg-mutedNight"] , []) [header, refs]
        replaceElements block = block
        header = Div ("", ["header"], []) 
          [ Para
            [ Span ("", ["las", "la-book", "mr-3"],[]) []
            , Str "References"
            ]
          ]

--------------------------------------------------------------------------------
