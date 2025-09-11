module Field.CanonicalUrl
( canonicalUrlField
) where

import Hakyll
import Data.List (isPrefixOf)
import System.FilePath (joinPath, normalise, splitFileName)

--------------------------------------------------------------------------------

canonicalUrlField :: String -> String -> Context String
canonicalUrlField name root = functionField name (\args item -> do
  route <- getRoute $ itemIdentifier item
  return $ case route of
    Nothing -> root
    Just r -> addRoot args r 
  )
  where addRoot [] route = route
        addRoot (url : _) route = if isExternal url then url else localUrl
          where localUrl = removeIndexHtml (root ++ toUrl (normalise combined))
                combined = if isRel url then joinPath ["/", route, url] else url
        isRel x = "./" `isPrefixOf` x || "../" `isPrefixOf` x

--------------------------------------------------------------------------------

removeIndexHtml :: String -> String
removeIndexHtml url = dir ++ ending
  where (dir, name) = splitFileName url
        ending
          | name == "index.html" = ""
          | otherwise = name


