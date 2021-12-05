module Field.Group.Year
( groupByYearField
) where

import Hakyll
import Data.List (groupBy)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (..))
import Data.Time.Locale.Compat (defaultTimeLocale)

--------------------------------------------------------------------------------

groupByYearField :: String -> [Item String] -> Context String -> Context String
groupByYearField name posts postContext = listField name ctx $ do
  tuples <- mapM extractYear posts
  let groups = makeItem . merge <$> groupByYear tuples
  sequence groups
  where ctx = field "year" (return . show . fst . itemBody)
           <> listFieldWith "posts" postContext (return . snd . itemBody)

--------------------------------------------------------------------------------

merge :: [(Integer, [Item String])] -> (Integer, [Item String])
merge gs = foldl1 conv (reverse gs)
  where conv (year, acc) (_, toAcc) = (year, toAcc ++ acc)


extractYear :: Item a -> Compiler (Integer,  [Item a])
extractYear item = do
   time <- getItemUTC defaultTimeLocale (itemIdentifier item)
   let    (year, _, _) = (toGregorian . utctDay) time
   return (year, [item])


groupByYear :: [(Integer, [Item String])] -> [[(Integer, [Item String])]]
groupByYear = groupBy (\(y, _) (y', _) -> y == y')
