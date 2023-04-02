{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Rules
  ( copy
  , css
  , index
  , posts
  , nz
  , templates
  ) where

--------------------------------------------------------------------------------

-- import Debug.Trace as D
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.Post as HSPost
import qualified Text.HTML.TagSoup.Compressor as TSCompressor
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Definition as PandocDef
import qualified Text.Pandoc.Walk as PandocWalk

--------------------------------------------------------------------------------

copy :: H.Rules ()
copy = do
  H.route H.idRoute
  H.compile H.copyFileCompiler

css :: H.Rules ()
css = do
  H.route H.idRoute
  H.compile H.compressCssCompiler

templates :: H.Rules ()
templates =
  H.compile H.templateBodyCompiler

--------------------------------------------------------------------------------
-- INDEX PAGE

index :: H.Rules ()
index = do
  H.route H.idRoute
  H.compile $ do
    loadedPosts <- H.recentFirst =<< H.loadAll "posts/*"

    let indexCtx = H.listField "posts" HSPost.postCtx (return loadedPosts)
                <> H.constField "root" HSConfig.mySiteRoot
                <> H.constField "feedTitle" HSConfig.myFeedTitle
                <> H.constField "siteName" HSConfig.mySiteName
                <> H.defaultContext

    H.getResourceBody
      >>= H.applyAsTemplate indexCtx
      >>= H.loadAndApplyTemplate "templates/default.html" indexCtx
      >>= compressHtmlCompiler

--------------------------------------------------------------------------------
-- POSTS

posts :: H.Rules ()
posts = do
  let ctx = H.constField "type" "article" <> HSPost.postCtx
  H.route $ H.metadataRoute HSPost.titleRoute
  H.compile $
    pandocCompilerCustom
      >>= H.saveSnapshot "content"
      >>= H.loadAndApplyTemplate "templates/note.html" ctx
      >>= H.loadAndApplyTemplate "templates/default.html" ctx
      >>= compressHtmlCompiler

-------------------------------------------------------------------------------
-- NZ

nz :: H.Rules ()
nz = do
  let ctx = H.constField "type" "article" <> HSPost.postCtx
  H.route $ H.setExtension "html"
  H.compile $
    pandocCompilerCustom
      >>= H.saveSnapshot "content"
      >>= H.loadAndApplyTemplate "templates/default_nz.html" ctx
      >>= compressHtmlCompiler

--------------------------------------------------------------------------------
-- HELPERS

pandocCompilerCustom :: H.Compiler (H.Item String)
pandocCompilerCustom =
  H.pandocCompilerWithTransformM
    pandocReaderOpts
    pandocWriterOpts
    customHighlight

---------
-- PANDOC

pandocReaderOpts :: Pandoc.ReaderOptions
pandocReaderOpts =
  H.defaultHakyllReaderOptions
    { Pandoc.readerExtensions = pandocExtensionsCustom
    }

pandocWriterOpts :: Pandoc.WriterOptions
pandocWriterOpts =
  H.defaultHakyllWriterOptions
    { Pandoc.writerExtensions = pandocExtensionsCustom
    }

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

-------------------
-- HTML COMPRESSION

compressHtmlCompiler :: H.Item String -> H.Compiler (H.Item String)
compressHtmlCompiler = pure . fmap compressHtml

compressHtml :: String -> String
compressHtml = H.withTagList TSCompressor.compress
