{-# LANGUAGE OverloadedStrings #-}

module Field.Tags
( allTagsField
, allTagsCloudField
) where

import Hakyll
import Control.Applicative (empty)
import Control.Monad (forM)
import Data.List (sortOn)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (Down(..))
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Util

--------------------------------------------------------------------------------

allTagsField :: String -> Tags -> Context String
allTagsField name tags = listFieldWith name (tagCtx tags) mkPostTags
  where
    tagCtx :: Tags -> Context String
    tagCtx tags = field "name" (return . itemBody)
               <> field "url" mkTagUrl

    mkTagUrl :: Item String -> Compiler String
    mkTagUrl item = toUrl <$> 
      ((<$>) fromJust . getRoute . tagsMakeId tags . itemBody $ item)

    mkPostTags :: Item String -> Compiler [Item String]
    mkPostTags item = (getTags . itemIdentifier $ item)
      >>= \tags' -> if null tags' then empty
                    else mapM makeItem tags'


allTagsCloudField :: Tags -> Context a
allTagsCloudField tags = field "tag-cloud" $ \_ -> do
  tags' <- forM (tagsMap tags) $ \(tag, ids) -> do
    route' <- getRoute $ tagsMakeId tags tag
    return ((tag, route'), length ids)
  let sortedTags = sortOn (Data.Ord.Down . \((_, _), i) -> i) tags'
  pure $ renderHtml $
    H.div ! A.id "tag-cloud"
          ! A.class_ "field is-grouped is-grouped-multiline" 
          $ toHtml $ map renderTagForCloud sortedTags

--------------------------------------------------------------------------------

renderTagForCloud :: ((String, Maybe FilePath), Int) -> H.Html
renderTagForCloud ((tagName, tagUrl), count) = 
  H.div ! A.class_ "control" $ toHtml $ 
    H.div ! A.class_ "tags has-addons" $ toHtml 
      [ H.a ! A.href (toValue url)
            ! A.class_ (H.stringValue $ "tag " ++ tagName)
            $ toHtml tagName 
      , H.span ! A.class_ "tag count" 
               $ toHtml count
      ]
  where url = toUrl $ fromMaybe "/" tagUrl
