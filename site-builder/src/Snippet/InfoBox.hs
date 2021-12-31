{-# LANGUAGE OverloadedStrings #-}

module Snippet.InfoBox
( isInfoBox
, toInfoBox
) where

import Hakyll
import Data.Maybe (fromMaybe, isJust)
import Text.Pandoc.Definition (Attr, Block(..), Inline(..))
import qualified Data.Text as T

import Snippet.Util

--------------------------------------------------------------------------------

isInfoBox :: Attr -> Bool
isInfoBox (_, classes, _) = isJust . getInfoBoxType $ classes


toInfoBox :: Block -> Block
toInfoBox (Div (id, classes, kvp) content) = Div (id, classes', []) content'
  where boxType = fromMaybe Notice $ getInfoBoxType classes
        classes' = classes ++ ["box", "fill-horizontal"]
        content' = makeTitle boxType kvp ++ content ++ makeCaption kvp

--------------------------------------------------------------------------------

data InfoBoxType = Notice | Warning | Help | Danger | Git | Itch


getInfoBoxType :: [T.Text] -> Maybe InfoBoxType
getInfoBoxType classes
  | "note" `elem` classes    = Just Notice
  | "warning" `elem` classes = Just Warning
  | "help" `elem` classes    = Just Help
  | "danger" `elem` classes  = Just Danger
  | "gitrepo" `elem` classes = Just Git
  | "itch" `elem` classes    = Just Itch
  | otherwise = Nothing


makeTitle :: InfoBoxType -> [(T.Text, T.Text)] -> [Block]
makeTitle boxType kvp = case lookup "header" kvp of
  Just header -> 
    [Div ("", ["header"], []) (contents header) ]
  Nothing -> []
  where contents header = [ Plain
          [ Span ("", [icon boxType, "mr-3"],[]) []
          , Str $ prefix boxType `T.append` header
          ]]


makeCaption :: [(T.Text, T.Text)] -> [Block]
makeCaption kvp = case  lookup "caption" kvp of
  Just caption -> [Div ("", 
    [ "caption"
    , "pt-3"
    , "border-t"
    , "border-textBlack"
    , "dark:border-textWhite"
    , "text-sm"
    ], []) (parse caption)]
  Nothing -> []


icon :: InfoBoxType -> T.Text
icon Notice = "las la-info-circle"
icon Warning = "las la-exclamation-circle"
icon Help = "las la-question-circle"
icon Danger = "las la-exclamation-circle"
icon Git = "lab la-github"
icon Itch = "lab la-itch-io"


prefix :: InfoBoxType -> T.Text
prefix Notice = "Note - "
prefix Warning = "Warning - "
prefix Danger = "Danger - "
prefix Git = "Repository - "
prefix _ = ""
