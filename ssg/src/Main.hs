{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger
import Hakyll
import System.FilePath (takeFileName)
import qualified Text.HTML.TagSoup as TS
import Text.Pandoc
  ( Extension (Ext_fenced_code_attributes, Ext_footnotes, Ext_gfm_auto_identifiers, Ext_implicit_header_references, Ext_smart),
    Extensions,
    ReaderOptions,
    WriterOptions (writerHighlightStyle),
    extensionsFromList,
    githubMarkdownExtensions,
    readerExtensions,
    writerExtensions,
  )
import Text.Pandoc.Highlighting (Style, breezeDark)

--------------------------------------------------------------------------------
-- PERSONALIZATION

mySiteName :: String
mySiteName = "Robert Pearce | Software Engineer"

mySiteRoot :: String
mySiteRoot = "https://robertwpearce.com"

myFeedTitle :: String
myFeedTitle = "Robert Pearce's blog"

myFeedDescription :: String
myFeedDescription = "Posts on JavaScript, Node.js, Haskell, Elm, Ruby and more."

myFeedAuthorName :: String
myFeedAuthorName = "Robert Pearce"

myFeedAuthorEmail :: String
myFeedAuthorEmail = "me@robertwpearce.com"

myFeedRoot :: String
myFeedRoot = mySiteRoot

--------------------------------------------------------------------------------
-- CONFIG

-- Default configuration: https://github.com/jaspervdj/hakyll/blob/cd74877d41f41c4fba27768f84255e797748a31a/lib/Hakyll/Core/Configuration.hs#L101-L125
config :: Configuration
config =
  defaultConfiguration
    { destinationDirectory = "dist"
    , ignoreFile = ignoreFile'
    , previewHost = "127.0.0.1"
    , previewPort = 8000
    , providerDirectory = "src"
    , storeDirectory = "ssg/_cache"
    , tmpDirectory = "ssg/_tmp"
    }
  where
    ignoreFile' path
      | ".DS_Store" == fileName = True
      | "."    `isPrefixOf` fileName = False
      | "#"    `isPrefixOf` fileName = True
      | "~"    `isSuffixOf` fileName = True
      | ".swp" `isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = hakyllWith config $ do
  forM_
    [ "CNAME"
    , "robots.txt"
    , "_config.yml"
    , ".well-known/*"
    , "js/build/*"
    , "images/*"
    , "fonts/*"
    , "pdfs/*"
    ]
    $ \f -> match f $ do
      route idRoute
      compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "posts/*" $ do
    let ctx = constField "type" "article" <> postCtx
    route $ metadataRoute titleRoute
    compile $
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  match "new-zealand/**" $ do
    let ctx = constField "type" "article" <> postCtx
    route $ setExtension "html"
    compile $
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/info.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"

      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "root" mySiteRoot
              <> constField "siteName" mySiteName
              <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= compressHtmlCompiler

  match "templates/*" $ compile templateBodyCompiler

  create ["sitemap.xml"] $ do
    route idRoute
    compile $ do
      posts   <- recentFirst =<< loadAll "posts/*"
      nzPages <- loadAll "new-zealand/**"

      let pages = posts <> nzPages
          sitemapCtx =
            constField "root" mySiteRoot
              <> constField "siteName" mySiteName
              <> listField "pages" postCtx (return pages)

      makeItem ("" :: String)
        >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  create ["rss.xml"] $ do
    route   idRoute
    compile (feedCompiler renderRss)

  create ["atom.xml"] $ do
    route   idRoute
    compile (feedCompiler renderAtom)

--------------------------------------------------------------------------------
-- COMPILER HELPERS

compressHtmlCompiler :: Item String -> Compiler (Item String)
compressHtmlCompiler = pure . fmap compressHtml

compressHtml :: String -> String
compressHtml = withTagList compressTags

compressTags :: [TS.Tag String] -> [TS.Tag String]
compressTags = go S.empty
  where
    go :: S.Set String -> [TS.Tag String] -> [TS.Tag String]
    go stack =
      \case [] -> []
            -- Removes comments by not prepending the tag
            -- and, instead, continuing on with the other tags
            ((TS.TagComment _str):rest) ->
              go stack rest

            -- When we find an open tag, like `<div>`, prepend it
            -- and continue through the rest of the tags while
            -- keeping a separate stack of what elements a given
            -- tag is currently "inside"
            (tag@(TS.TagOpen name _attrs):rest) ->
              tag : go (S.insert name stack) rest

            -- When we find a closing tag, like `</div>`, prepend it
            -- it and continue through the rest of the tags, making
            -- sure to remove it from our stack of currently opened
            -- elements
            (tag@(TS.TagClose name):rest) ->
              tag : go (S.delete name stack) rest

            -- When a text/string tag is encountered, if it has
            -- significant whitespace that should be preserved,
            -- then prepend it without change; otherwise, clean up
            -- the whitespace, and prepend it
            (tag@(TS.TagText _str):rest)
              | hasSignificantWhitespace stack -> tag : go stack rest
              | otherwise -> fmap cleanWhitespace tag : go stack rest

            -- If none of the above match, then this is unexpected,
            -- so we should prepend the tag without change
            (tag:rest) ->
              tag : go stack rest

    -- Whitespace-sensitive content that shouldn't be compressed
    hasSignificantWhitespace :: S.Set String -> Bool
    hasSignificantWhitespace stack =
      any (`S.member` stack) content
      where
        content = [ "pre", "textarea" ]

    cleanWhitespace :: String -> String
    cleanWhitespace " " = " "
    cleanWhitespace str = cleanWS str (clean str)
      where
        -- Strips out newlines, spaces, etc
        clean :: String -> String
        clean = unwords . words

        -- Clean the whitespace while preserving
        -- single leading and trailing whitespace
        -- characters when it makes sense
        cleanWS :: String -> String -> String
        cleanWS _originalStr "" = ""
        cleanWS originalStr trimmedStr =
          keepSpaceWhen head originalStr ++
            trimmedStr ++
            keepSpaceWhen last originalStr

        -- Determine when to keep a space based on a
        -- string and a function that returns a character
        -- within that string
        keepSpaceWhen :: ([Char] -> Char) -> String -> String
        keepSpaceWhen _fn ""  = ""
        keepSpaceWhen fn originalStr
          | (isSpace . fn) originalStr = " "
          | otherwise = ""

-- https://rebeccaskinner.net/posts/2021-01-31-hakyll-syntax-highlighting.html
--makeStyle :: Style -> Compiler (Item String)
--makeStyle =
--  makeItem . compressCss . styleToCss

--------------------------------------------------------------------------------
-- CONTEXT

feedCtx :: Context String
feedCtx =
  titleCtx
    <> postCtx
    <> bodyField "description"

postCtx :: Context String
postCtx =
  constField "root" mySiteRoot
    <> constField "siteName" mySiteName
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext

titleCtx :: Context String
titleCtx =
  field "title" updatedTitle

--------------------------------------------------------------------------------
-- TITLE HELPERS

replaceAmp :: String -> String
replaceAmp =
  replaceAll "&" (const "&amp;")

replaceTitleAmp :: Metadata -> String
replaceTitleAmp =
  replaceAmp . safeTitle

safeTitle :: Metadata -> String
safeTitle =
  fromMaybe "no title" . lookupString "title"

updatedTitle :: Item a -> Compiler String
updatedTitle =
  fmap replaceTitleAmp . getMetadata . itemIdentifier

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  pandocCompilerWith pandocReaderOpts pandocWriterOpts

pandocExtensionsCustom :: Extensions
pandocExtensionsCustom =
  githubMarkdownExtensions
    <> extensionsFromList
      [ Ext_fenced_code_attributes
      , Ext_gfm_auto_identifiers
      , Ext_implicit_header_references
      , Ext_smart
      , Ext_footnotes
      ]

pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
  defaultHakyllReaderOptions
    { readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: WriterOptions
pandocWriterOpts =
  defaultHakyllWriterOptions
    { writerExtensions = pandocExtensionsCustom
    , writerHighlightStyle = Just pandocHighlightStyle
    }

pandocHighlightStyle :: Style
pandocHighlightStyle =
  breezeDark -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

--------------------------------------------------------------------------------
-- FEEDS

type FeedRenderer
  = FeedConfiguration
  -> Context String
  -> [Item String]
  ->  Compiler (Item String)

feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< recentFirst
    =<< loadAllSnapshots "posts/*" "content"

feedConfiguration :: FeedConfiguration
feedConfiguration =
  FeedConfiguration
    { feedTitle = myFeedTitle
    , feedDescription = myFeedDescription
    , feedAuthorName = myFeedAuthorName
    , feedAuthorEmail = myFeedAuthorEmail
    , feedRoot = myFeedRoot
    }

--------------------------------------------------------------------------------
-- CUSTOM ROUTE

getTitleFromMeta :: Metadata -> String
getTitleFromMeta =
  fromMaybe "no title" . lookupString "title"

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . getTitleFromMeta

titleRoute :: Metadata -> Routes
titleRoute =
  constRoute . fileNameFromTitle
