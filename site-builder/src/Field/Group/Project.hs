module Field.Group.Project
( groupByProjectField
) where

import Hakyll
import Data.List (find, groupBy)
import Data.Maybe (isJust, maybeToList)

import Config
import Util

--------------------------------------------------------------------------------

groupByProjectField :: String -> [Item String] -> [Item String] -> Context String -> Context String
groupByProjectField name posts projects context = listField name ctx $ do
  tuples <- mapM (extractProjectSlug projects) posts
  let groups = makeItem . merge <$> groupBySlug projects tuples
  sequence groups
  where ctx = listFieldWith "project" context (pure . getProject projects . fst . itemBody)
           <> listFieldWith "posts" context (pure . snd . itemBody)

--------------------------------------------------------------------------------

type Slug = String
type Post = Item String


merge :: [(Slug, [Post])] -> (Slug, [Post])
merge gs = foldl1 conv (reverse gs)
  where conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)


extractProjectSlug :: [Post] -> Post -> Compiler (Slug, [Post])
extractProjectSlug projects item = do
  m <- getMetadata $ itemIdentifier item
  let mSlug = lookupString "project" m
  pure (concat $ maybeToList mSlug, [item])


getProject :: [Post] -> Slug -> [Post]
getProject projects slug = maybeToList $ find isProject projects
  where isProject project = slug == getSlug project


isValidProject :: [Post] -> (Slug, [Post]) -> Bool
isValidProject projects (slug, [item])
  | null slug = False
  | otherwise = isJust $ find (\p -> getSlug p == slug) projects
isValidProject _ (_, []) = False


groupBySlug :: [Post] -> [(Slug, [Post])] -> [[(Slug, [Post])]]
groupBySlug projects posts = groupBy (\(y, _) (y', _) -> y == y') $ 
  filter (isValidProject projects) posts
