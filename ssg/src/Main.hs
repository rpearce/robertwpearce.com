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
import qualified Hakyll as H
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
config :: H.Configuration
config =
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
        fileName = FilePath.takeFileName path

--------------------------------------------------------------------------------
-- BUILD

main :: IO ()
main = H.hakyllWith config $ do
  Monad.forM_
    [ "CNAME"
    , "robots.txt"
    , "_config.yml"
    , ".well-known/*"
    , "images/*"
    , "fonts/*"
    , "pdfs/*"
    ]
    $ \f -> H.match f $ do
      H.route H.idRoute
      H.compile H.copyFileCompiler

  H.match "css/*" $ do
    H.route H.idRoute
    H.compile H.compressCssCompiler

  H.match "notes/*" $ do
    let ctx = H.constField "type" "article" <> postCtx
    H.route $ H.metadataRoute titleRoute
    H.compile $
      pandocCompilerCustom
        >>= H.loadAndApplyTemplate "templates/note.html" ctx
        >>= H.saveSnapshot "content"
        >>= H.loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  H.match "new-zealand/**" $ do
    let ctx = H.constField "type" "article" <> postCtx
    H.route $ H.setExtension "html"
    H.compile $
      pandocCompilerCustom
        >>= H.saveSnapshot "content"
        >>= H.loadAndApplyTemplate "templates/default.html" ctx
        >>= compressHtmlCompiler

  H.match "index.html" $ do
    H.route H.idRoute
    H.compile $ do
      posts <- H.recentFirst =<< H.loadAll "notes/*"

      let indexCtx =
            H.listField "posts" postCtx (return posts)
              <> H.constField "root" mySiteRoot
              <> H.constField "feedTitle" myFeedTitle
              <> H.constField "siteName" mySiteName
              <> H.defaultContext

      H.getResourceBody
        >>= H.applyAsTemplate indexCtx
        >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
        >>= compressHtmlCompiler

  H.match "templates/*" $ H.compile H.templateBodyCompiler

  H.create ["sitemap.xml"] $ do
    H.route H.idRoute
    H.compile $ do
      posts   <- H.recentFirst =<< H.loadAll "notes/*"
      nzPages <- H.loadAll "new-zealand/**"

      let pages = posts <> nzPages
          sitemapCtx =
            H.constField "root" mySiteRoot
              <> H.constField "siteName" mySiteName
              <> H.listField "pages" postCtx (return pages)

      H.makeItem ("" :: String)
        >>= H.loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

  H.create ["rss.xml"] $ do
    H.route   H.idRoute
    H.compile (feedCompiler H.renderRss)

  H.create ["atom.xml"] $ do
    H.route   H.idRoute
    H.compile (feedCompiler H.renderAtom)

--------------------------------------------------------------------------------
-- COMPILER HELPERS

compressHtmlCompiler :: H.Item String -> H.Compiler (H.Item String)
compressHtmlCompiler = pure . fmap compressHtml

compressHtml :: String -> String
compressHtml = H.withTagList TSCompressor.compress

--------------------------------------------------------------------------------
-- CONTEXT

feedCtx :: H.Context String
feedCtx =
  titleCtx
    <> postCtx
    <> H.bodyField "description"

postCtx :: H.Context String
postCtx =
  H.constField "root" mySiteRoot
    <> H.constField "feedTitle" myFeedTitle
    <> H.constField "siteName" mySiteName
    <> H.dateField "date" "%Y-%m-%d"
    <> H.defaultContext

titleCtx :: H.Context String
titleCtx =
  H.field "title" updatedTitle

--------------------------------------------------------------------------------
-- TITLE HELPERS

replaceAmp :: String -> String
replaceAmp =
  H.replaceAll "&" (const "&amp;")

replaceTitleAmp :: H.Metadata -> String
replaceTitleAmp =
  replaceAmp . safeTitle

safeTitle :: H.Metadata -> String
safeTitle =
  Maybe.fromMaybe "no title" . H.lookupString "title"

updatedTitle :: H.Item a -> H.Compiler String
updatedTitle =
  fmap replaceTitleAmp . H.getMetadata . H.itemIdentifier

--------------------------------------------------------------------------------
-- PANDOC

pandocCompilerCustom :: H.Compiler (H.Item String)
pandocCompilerCustom =
  H.pandocCompilerWithTransformM
    pandocReaderOpts
    pandocWriterOpts
    customHighlight

-- https://tony-zorman.com/posts/pygmentising-hakyll.html
customHighlight :: PandocDef.Pandoc -> H.Compiler PandocDef.Pandoc
customHighlight =
  PandocWalk.walkM \case
    PandocDef.CodeBlock (_, Maybe.listToMaybe -> mbLang, _) (T.unpack -> body) -> do
      let lang = T.unpack (Maybe.fromMaybe "text" mbLang)
      PandocDef.RawBlock "html" . T.pack . withWrapElement <$> callHighlighter lang body
    block -> pure block
  where
    -- https://github.com/alecthomas/chroma
    callHighlighter :: String -> String -> H.Compiler String
    callHighlighter lang =
      H.unixFilter "chroma"
        [ "--html"
        , "--html-only"
        , "--html-prefix=pl-"
        , "--lexer=" ++ lang
        ]

    -- Turning withWrapElement effectively into `id` until
    -- I need to wrap `<pre>`s in another element
    withWrapElement :: String -> String
    withWrapElement html = html -- "<div class=\"code-block\">" ++ html ++ "</div>"

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
  H.defaultHakyllReaderOptions
    { Pandoc.readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: Pandoc.WriterOptions
pandocWriterOpts =
  H.defaultHakyllWriterOptions
    { Pandoc.writerExtensions = pandocExtensionsCustom
    --, writerHighlightStyle = Just pandocHighlightStyle
    }

--pandocHighlightStyle :: Style
--pandocHighlightStyle =
--  pygments -- https://hackage.haskell.org/package/pandoc/docs/Text-Pandoc-Highlighting.html

--------------------------------------------------------------------------------
-- FEEDS

type FeedRenderer
  = H.FeedConfiguration
  -> H.Context String
  -> [H.Item String]
  ->  H.Compiler (H.Item String)

feedCompiler :: FeedRenderer -> H.Compiler (H.Item String)
feedCompiler renderer =
  renderer feedConfiguration feedCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "notes/*" "content"

feedConfiguration :: H.FeedConfiguration
feedConfiguration =
  H.FeedConfiguration
    { H.feedTitle = myFeedTitle
    , H.feedDescription = myFeedDescription
    , H.feedAuthorName = myFeedAuthorName
    , H.feedAuthorEmail = myFeedAuthorEmail
    , H.feedRoot = myFeedRoot
    }

--------------------------------------------------------------------------------
-- CUSTOM ROUTE

getTitleFromMeta :: H.Metadata -> String
getTitleFromMeta =
  Maybe.fromMaybe "no title" . H.lookupString "title"

fileNameFromTitle :: H.Metadata -> FilePath
fileNameFromTitle =
  T.unpack . (`T.append` ".html") . Slugger.toSlug . T.pack . getTitleFromMeta

titleRoute :: H.Metadata -> H.Routes
titleRoute =
  H.constRoute . fileNameFromTitle
