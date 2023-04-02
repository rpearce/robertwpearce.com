module Hakyll.Site.Configuration
  ( SiteConfiguration (..)
  , H.FeedConfiguration (..)
  , feedConfiguration
  , hakyllConfiguration
  , siteConfiguration
  , mySiteName
  , mySiteRoot
  , myFeedTitle
  , myFeedDescription
  , myFeedAuthorName
  , myFeedAuthorEmail
  , myFeedRoot
  ) where

--------------------------------------------------------------------------------

import qualified Data.List as List
import qualified Hakyll as H
import qualified System.FilePath as FP

--------------------------------------------------------------------------------

data SiteConfiguration = SiteConfiguration
  { siteName :: String
  , siteRoot :: String
  } deriving (Show)

siteConfiguration :: SiteConfiguration
siteConfiguration =
  SiteConfiguration
    { siteName = "Robert Pearce"
    , siteRoot = "https://robertwpearce.com"
    }

--------------------------------------------------------------------------------

-- https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L69-L81
feedConfiguration :: H.FeedConfiguration
feedConfiguration =
  H.FeedConfiguration
    { H.feedTitle = "Robert Pearce's blog"
    , H.feedDescription = "Posts on JavaScript, Nix, Haskell, Ruby, Bash, and more."
    , H.feedAuthorName = "Robert Pearce"
    , H.feedAuthorEmail = "me@robertwpearce.com"
    , H.feedRoot = "https://robertwpearce.com"
    }

-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
hakyllConfiguration :: H.Configuration
hakyllConfiguration =
  H.defaultConfiguration
    { H.destinationDirectory = "dist"
    , H.ignoreFile = ignoreFile'
    , H.previewHost = "127.0.0.1"
    , H.previewPort = 8000
    , H.providerDirectory = "src"
    , H.storeDirectory = "ssg/_cache"
    , H.tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | ".DS_Store" == fileName = True
      | "."    `List.isPrefixOf` fileName = False
      | "#"    `List.isPrefixOf` fileName = True
      | "~"    `List.isSuffixOf` fileName = True
      | ".swp" `List.isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = FP.takeFileName path

--------------------------------------------------------------------------------

mySiteName :: String
mySiteName = siteName siteConfiguration

mySiteRoot :: String
mySiteRoot = siteRoot siteConfiguration

myFeedTitle :: String
myFeedTitle = H.feedTitle feedConfiguration

myFeedDescription :: String
myFeedDescription = H.feedDescription feedConfiguration

myFeedAuthorName :: String
myFeedAuthorName = H.feedAuthorName feedConfiguration

myFeedAuthorEmail :: String
myFeedAuthorEmail = H.feedAuthorEmail feedConfiguration

myFeedRoot :: String
myFeedRoot = H.feedRoot feedConfiguration
