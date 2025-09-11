{-# LANGUAGE OverloadedStrings #-}

module Field
( module Field.Bibliography
, module Field.CanonicalUrl
, module Field.Git
, module Field.Group
, module Field.List
, module Field.Markdown
, module Field.TableOfContents
, module Field.Tags
, slugField
, peekField
, timeField
, concatField
) where

import Hakyll
import System.FilePath.Posix (takeBaseName)
import qualified Data.Text as T

import Field.Bibliography
import Field.CanonicalUrl
import Field.Git
import Field.Group
import Field.List
import Field.Markdown
import Field.TableOfContents
import Field.Tags
import Util

--------------------------------------------------------------------------------

slugField :: String -> Context String
slugField name = field name $ pure . getSlug


peekField :: Int -> String -> Snapshot -> Context String
peekField length name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  return (peak body)
  where peak = T.unpack . T.unwords . take length . T.words . T.pack


timeField :: String -> Snapshot -> Context String
timeField name snapshot = field name $ \item -> do
  body <- itemBody <$> loadSnapshot (itemIdentifier item) snapshot
  let words = length (T.words . T.pack $ body)
  return $ show $ div words 200


concatField :: String -> Context String
concatField name = functionField name $ \args item -> return $ concat args

--------------------------------------------------------------------------------
