{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
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
            ((TS.TagComment _):rest) -> go stack rest
            (tag@(TS.TagOpen name _):rest) -> tag : go (S.insert name stack) rest
            (tag@(TS.TagClose name):rest) -> tag : go (S.delete name stack) rest
            (tag@(TS.TagText _):rest)
              | hasSignificantWhitespace stack -> tag : go stack rest
              | hasTextContent stack -> fmap cleanTabsNewLines tag : go stack rest
              | otherwise -> fmap cleanAll tag : go stack rest
            (tag:rest) -> tag : go stack rest

    -- Whitespace-sensitive content that shouldn't be compressed
    hasSignificantWhitespace :: S.Set String -> Bool
    hasSignificantWhitespace stack =
      any (`S.member` stack) content
      where
        content = [ "pre", "script", "textarea" ]

    -- Elements that can hold text content and should
    -- hold on to leading and trailing whitespace
    hasTextContent :: S.Set String -> Bool
    hasTextContent stack = any (`S.member` stack) content
      where
        content =
          [ "a", "abbr", "b", "bdi", "bdo", "blockquote", "button", "cite"
          , "code", "del", "dfn", "em", "figcaption", "i", "img", "input", "ins"
          , "kbd", "label", "li", "mark", "math", "noscript", "object", "p"
          , "picture", "q", "rp", "rt", "ruby", "s", "samp", "select", "small"
          , "span", "strong", "sub", "sup", "svg", "td", "textarea", "time"
          , "var", "wbr"
          ]

    -- Replace tab characters with spaces
    replaceTab :: Char -> Char
    replaceTab '\t' = ' '
    replaceTab s    = s

    -- Replace newline characters with spaces
    replaceNewLine :: Char -> Char
    replaceNewLine '\n' = ' '
    replaceNewLine s    = s

    -- Remove the following:
    --   '\f' (form feed)
    --   '\n' (newline [line feed])
    --   '\r' (carriage return)
    --   '\v' (vertical tab)
    rmNewLines :: String -> String
    rmNewLines = filter (not . (`elem` ("\f\n\r\v" :: String)))

    cleanTabsNewLines :: String -> String
    cleanTabsNewLines = fmap (replaceNewLine . replaceTab)

    cleanAll :: String -> String
    cleanAll = rmNewLines . trim . fmap replaceTab

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
