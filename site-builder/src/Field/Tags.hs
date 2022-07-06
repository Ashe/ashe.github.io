module Field.Tags
( allTagsField
, allTagsCloudField
) where

import Hakyll
import Control.Applicative (empty)
import Data.Maybe (fromJust)
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
allTagsCloudField tags = tagCloudField "tag-cloud" 110 550 (randomiseTags tags)

--------------------------------------------------------------------------------

randomiseTags :: Tags -> Tags
randomiseTags tags = tags { tagsMap = shuffle $ tagsMap tags }
