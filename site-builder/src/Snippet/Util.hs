module Snippet.Util
( hasClass
, hasAnyClass
, parse
) where

import Hakyll

import Text.Pandoc (runPure)
import Text.Pandoc.Definition (Pandoc (..), Attr, Block(..))
import Text.Pandoc.Readers (readMarkdown)
import qualified Data.Text as T

--------------------------------------------------------------------------------

hasClass :: T.Text -> Attr -> Bool
hasClass search (_, classes, _) = search `elem` classes


hasAnyClass :: [T.Text] -> Attr -> Bool
hasAnyClass search (_, classes, _) = hasAny search classes


parse :: T.Text -> [Block]
parse body = case runPure $ readMarkdown defaultHakyllReaderOptions body of
    Left err -> error "Could not parse"
    Right (Pandoc meta blocks) -> blocks

--------------------------------------------------------------------------------

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny [] _          = False
hasAny _ []          = False
hasAny search (x:xs) = x `elem` search || hasAny search xs
