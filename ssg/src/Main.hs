{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- import Debug.Trace as D
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Data.Text.Slugger as Slugger
import Hakyll
import qualified System.FilePath as FilePath
import qualified Text.HTML.TagSoup.Compressor as TSCompressor
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as PandocDef
import qualified Text.Pandoc.Walk as PandocWalk

--------------------------------------------------------------------------------
-- PERSONALIZATION

mySiteName :: String
mySiteName = "Robert Pearce"

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
      | "."    `List.isPrefixOf` fileName = False
      | "#"    `List.isPrefixOf` fileName = True
      | "~"    `List.isSuffixOf` fileName = True
      | ".swp" `List.isSuffixOf` fileName = True
      | otherwise = False
      where
        fileName = FilePath.takeFileName path

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = hakyllWith config $ do
  Monad.forM_
    [ "CNAME"
    , "robots.txt"
    , "_config.yml"
    , ".well-known/*"
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

  match "notes/*" $ do
    let ctx = constField "type" "article" <> postCtx
    route $ metadataRoute titleRoute
    compile $
      pandocCompilerCustom
        >>= loadAndApplyTemplate "templates/note.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  match "new-zealand/**" $ do
    let ctx = constField "type" "article" <> postCtx
    route $ setExtension "html"
    compile $
      pandocCompilerCustom
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "notes/*"

      let indexCtx =
            listField "posts" postCtx (return posts)
              <> constField "root" mySiteRoot
              <> constField "feedTitle" myFeedTitle
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
      posts   <- recentFirst =<< loadAll "notes/*"
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
compressHtml = withTagList TSCompressor.compress

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
    <> constField "feedTitle" myFeedTitle
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
  Maybe.fromMaybe "no title" . lookupString "title"

updatedTitle :: Item a -> Compiler String
updatedTitle =
  fmap replaceTitleAmp . getMetadata . itemIdentifier

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
  pandocCompilerWithTransformM
    pandocReaderOpts
    pandocWriterOpts
    customHighlight

-- https://tony-zorman.com/posts/pygmentising-hakyll.html
customHighlight :: PandocDef.Pandoc -> Compiler PandocDef.Pandoc
customHighlight =
  PandocWalk.walkM \case
    PandocDef.CodeBlock (_, Maybe.listToMaybe -> mbLang, _) (T.unpack -> body) -> do
      let lang = T.unpack (Maybe.fromMaybe "text" mbLang)
      PandocDef.RawBlock "html" . T.pack <$> callHighlighter lang body
    block -> pure block
  where
    -- https://github.com/alecthomas/chroma
    callHighlighter :: String -> String -> Compiler String
    callHighlighter lang =
      unixFilter "chroma"
        [ "--html"
        , "--html-only"
        , "--lexer=" ++ lang
        ]

pandocExtensionsCustom :: Pandoc.Extensions
pandocExtensionsCustom =
  Pandoc.githubMarkdownExtensions
    <> Pandoc.extensionsFromList
      [ Pandoc.Ext_fenced_code_attributes
      , Pandoc.Ext_gfm_auto_identifiers
      , Pandoc.Ext_implicit_header_references
      , Pandoc.Ext_smart
      , Pandoc.Ext_footnotes
      ]

pandocReaderOpts :: Pandoc.ReaderOptions
pandocReaderOpts =
  defaultHakyllReaderOptions
    { Pandoc.readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: Pandoc.WriterOptions
pandocWriterOpts =
  defaultHakyllWriterOptions
    { Pandoc.writerExtensions = pandocExtensionsCustom
    --, writerHighlightStyle = Just pandocHighlightStyle
    }

--pandocHighlightStyle :: Style
--pandocHighlightStyle =
--  pygments -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

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
    =<< loadAllSnapshots "notes/*" "content"

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
  Maybe.fromMaybe "no title" . lookupString "title"

fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . getTitleFromMeta

titleRoute :: Metadata -> Routes
titleRoute =
  constRoute . fileNameFromTitle
