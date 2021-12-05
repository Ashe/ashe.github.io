module Field.TableOfContents
( tableOfContentsField
) where

import Hakyll

import Data.Default (Default, def)
import Data.Either (fromRight)
import Data.String (fromString)
import Text.Blaze.Html (Html, Attribute)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Internal ( MarkupM (..), (!))
import Text.Blaze.XHtml5 (ul, li, toHtml)
import Text.Blaze.XHtml5.Attributes (class_, alt)
import Text.Pandoc (Pandoc (..), Block (BulletList), nullMeta, runPure)
import Text.Pandoc.Options (WriterOptions (writerTOCDepth))
import Text.Pandoc.Readers (readHtml)
import Text.Pandoc.Writers (writeHtml5)
import Text.Pandoc.Writers.Shared (toTableOfContents)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

--------------------------------------------------------------------------------

-- Create a table of contents for the current file
tableOfContentsField :: String -> Int -> ToCExtra -> String -> Context String
tableOfContentsField name depth tocExtra snapshot = field name $ \item -> do
  body <- loadSnapshot (itemIdentifier item) snapshot
  let writerOptions = def { writerTOCDepth = depth }
      pandoc@(Pandoc _ blocks) = 
        case runPure $ readHtml defaultHakyllReaderOptions (T.pack $ itemBody body) of
          Left err    -> error "Could not parse"
          Right pandoc -> pandoc
      toc = toTableOfContents writerOptions blocks
      ulAttributes ul' = ul' ! class_ (fromString $ extraUlClasses tocExtra)
  pure . TL.unpack . renderHtml . modList writerOptions ulAttributes $ [toc]

--------------------------------------------------------------------------------

newtype ToCExtra =  ToCExtra { extraUlClasses :: String }
     deriving (Show)

instance Default ToCExtra where
    def = ToCExtra { extraUlClasses = "" }


modList :: WriterOptions -> ((Html -> Html)->(Html->Html)) -> [Block] -> Html
modList opts ulMod = makeBulletItem
  where
      -- This decomposes one item of a bullet list
      -- BulletList takes a  list of items each of which is a list of blocks
      -- respectively :: [[Block]]
      makeBulletItem :: [Block] -> Html
      makeBulletItem [] = Empty ()
      makeBulletItem ((BulletList elems):extra)
        = toHtml [makeList $ filter (not . null) elems, makeBulletItem extra]
      makeBulletItem (block:extra) = toHtml [makeItem block, makeBulletItem extra]

      makeItem :: Block -> Html
      makeItem block = fromRight (Empty ()) (runPure $ writeHtml5 opts (Pandoc nullMeta [block]))

      makeList:: [[Block]] -> Html
      makeList [] = Empty ()
      makeList listItems = ulMod ul (toHtml . Prelude.map (li . makeBulletItem) $ listItems)
