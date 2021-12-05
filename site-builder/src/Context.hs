module Context
( siteContext
, feedContext
, contentContext
, blogPostContext
, projectContext
) where

import Hakyll
import Data.Default (def)

import Config
import Field

--------------------------------------------------------------------------------

siteContext :: Context String
siteContext = headCommitField "git-head-commit" Commit
           <> headCommitField "git-head-commit-hash" Hash
           <> headCommitField "git-head-commit-full" Full
           <> constField "item-type" "default"
           <> concatField "concat"
           <> markdownField "read-md"
           <> slugField "slug"
           <> atIndexField "at-index" 
           <> socialContext
           <> siteDetailsContext
           <> defaultContext


feedContext :: Context String
feedContext = bodyField "description"
           <> dateField "date" "%Y-%m-%d"
           <> siteContext


contentContext :: Tags -> Tags -> Context String
contentContext tags category = dateField "date" "%B %e, %Y"
                            <> allTagsField "tags" tags
                            <> allTagsField "category" category
                            <> simpleListField "authors" "author"
                            <> simpleListField "images" "image"
                            <> constField "item-type" "post"
                            <> teaserField "teaser" "posts-content"
                            <> peekField 50 "peek" "posts-content"
                            <> timeField "read-time" "posts-content"
                            <> tableOfContentsField "toc" 4 def "posts-content"
                            <> pathField "sourcefile"
                            <> commitField "git-commit" Commit
                            <> commitField "git-commit-hash" Hash
                            <> siteContext


blogPostContext :: Tags -> Tags -> Context String
blogPostContext = contentContext


projectContext :: Tags -> Tags -> Context String
projectContext = contentContext

--------------------------------------------------------------------------------

siteDetailsContext :: Context String
siteDetailsContext = constField "root" root
                  <> canonicalUrlField "to-canonical" root
                  <> constField "site-description" siteDescription
                  <> constField "site-logo" siteLogo
                  <> constField "site-repo" siteRepo
                  <> constField "profile-photo" socialProfilePicture


socialContext :: Context String
socialContext = constField "social-name" socialName
             <> constField "social-email" socialEmail
             <> constField "social-github" socialGithub
             <> constField "social-linkedin" socialLinkedin
             <> constField "social-devto" socialDevto
             <> constField "social-instagram" socialInstagram
