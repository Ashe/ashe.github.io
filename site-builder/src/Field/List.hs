module Field.List
( atIndexField
, isNotEmptyField
, simpleListField
) where

import Hakyll
import Control.Applicative (empty)
import Data.Maybe (maybeToList)

--------------------------------------------------------------------------------

atIndexField :: String -> Context String
atIndexField name = functionField name f
  where f (subName : index : _) item = do
          m <- getMetadata $ itemIdentifier item
          pure $ case lookupStringList subName m of
            Just list -> 
              if length list >= read index then list !! read index
              else []
            _ -> []
        f _ _ = pure []


isNotEmptyField :: String -> String -> Context String
isNotEmptyField name subFieldName = boolFieldWith name f
  where f item = do
          m <- getMetadata $ itemIdentifier item
          case lookupStringList subFieldName m of
            Just list -> pure . not . null $ list
            _ -> pure False


simpleListField :: String -> String -> Context String
simpleListField name subFieldName = 
  listFieldWith name context makeItems
  <> isNotEmptyField ("has-" ++ name) name
  where context = field subFieldName (pure . fst . itemBody)
               <> field "index" (pure . show . snd . itemBody)
               <> metadataField
        makeItems item = do
          m <- getMetadata $ itemIdentifier item
          let allItemData = concat $ maybeToList $ lookupStringList name m
          mapM makeItem $ zip allItemData [0..]

--------------------------------------------------------------------------------

field' :: String -> (Item a -> Compiler ContextField) -> Context a
field' key value = Context $ \k args i -> if k == key then value i else empty

boolFieldWith :: String -> (Item b -> Compiler Bool) -> Context b
boolFieldWith key  f = field' key $ \i -> do
  result <- f i
  if result then pure (error "hello") else empty
