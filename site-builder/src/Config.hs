{-# LANGUAGE OverloadedStrings #-}

module Config 
( sourceDir
, domain
, root
, siteRepo
, siteDescription
, siteLogo
, socialName
, socialProfilePicture
, socialEmail
, socialGithub
, socialLinkedin
, socialInstagram
, postsGlob
, projectsGlob
, contentGlob
, jpgs
, svg
, readerOptions
) where

import Hakyll
import Text.Pandoc (ReaderOptions (readerExtensions), def, enableExtension)
import Text.Pandoc.Options (Extension (Ext_citations))

-- Site configuration
--------------------------------------------------------------------------------

sourceDir :: String
sourceDir = "data/"

domain :: String
domain = "aas.sh"

root :: String
root = "https://" ++ domain

-- Site details
--------------------------------------------------------------------------------

siteRepo :: String
siteRepo = "https://github.com/ashe/ashe.github.io"

siteDescription :: String
siteDescription = "Hey there! My name is Ashley Smith and I'm a game developer from the UK. I love to undertake projects to improve, challenge and extend my skillset - I never stop learning."

siteLogo :: String
siteLogo = "/assets/images/logo.png"

-- Socials
--------------------------------------------------------------------------------

socialName :: String
socialName = "Ashley Smith"

socialProfilePicture :: String
socialProfilePicture = "https://res.cloudinary.com/aas-sh/image/upload/v1623408029/site/profile_square.jpg"

socialEmail :: String
socialEmail = "contact@aas.sh"

socialGithub :: String
socialGithub = "ashe"

socialLinkedin :: String
socialLinkedin = "itsashe"

socialInstagram :: String
socialInstagram = "theofficialashe"

-- Patterns
--------------------------------------------------------------------------------

postsGlob :: Pattern
postsGlob = "content/blog/**.md"

projectsGlob :: Pattern
projectsGlob = "content/projects/**.md"

contentGlob :: Pattern
contentGlob = postsGlob .||. projectsGlob

jpgs :: Pattern
jpgs = "**.jpg" .||. "**.jpeg"

svg :: Pattern
svg = "**.svg"

-- Configuration
--------------------------------------------------------------------------------

readerOptions:: ReaderOptions
readerOptions = def {
  readerExtensions = enableExtension Ext_citations (readerExtensions defaultHakyllReaderOptions)
}
