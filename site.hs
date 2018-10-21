{-# LANGUAGE OverloadedStrings #-}
--import qualified Data.Map          as M
import           Data.Maybe           (fromMaybe)
import           Hakyll
import           Hakyll.Core.Metadata (lookupString)
import           Hakyll.Web.Pandoc
import           Text.Pandoc
import           Web.Slug             (mkSlug)

import           Data.Char            (isAlphaNum)
import           Data.Text            (Text)
import qualified Data.Text            as T


main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler


    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler


    match "posts/*" $ do
        let ctx = constField "type" "article" <> postCtx

        route $ metadataRoute customizedRoute
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/post.html"    ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Home" <>
                    constField "root" root <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx


    match "templates/*" $ compile templateBodyCompiler


    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let sitemapCtx = listField "posts" postCtx (return posts)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = titleContext <> postCtx <> bodyField "description"
            posts <- recentFirst =<< loadAll "posts/*"
            renderAtom feedConfiguration feedCtx posts


-- CONFIG


root :: String
root =
    "https://robertwpearce.com"


config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "docs"
        , previewHost          = "127.0.0.1"
        , previewPort          = 8000
        }


-- CONTEXT


replaceAmp :: String -> String
replaceAmp =
    replaceAll "&" (const "&amp;")


titleContext :: Context a
titleContext =
    field "title" $ \item -> do
        metadata <- getMetadata (itemIdentifier item)
        return $ replaceAmp $ fromMaybe "no title" $ lookupString "title" metadata


postCtx :: Context String
postCtx =
    dateField "date" "%b %e, %Y" <>
    constField "root" root <>
    defaultContext


-- PANDOC


pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
    pandocCompilerWith pandocReaderOpts pandocWriterOpts


pandocReaderOpts :: ReaderOptions
pandocReaderOpts =
    defaultHakyllReaderOptions
        { readerExtensions = enableExtension Ext_smart githubMarkdownExtensions
        }


pandocWriterOpts :: WriterOptions
pandocWriterOpts =
    defaultHakyllWriterOptions
        { writerExtensions = enableExtension Ext_smart githubMarkdownExtensions
        }


-- FEEDS


feedConfiguration :: FeedConfiguration
feedConfiguration =
    FeedConfiguration
        { feedTitle       = "Robert Pearce's blog"
        , feedDescription = "Posts on JavaScript, Node.js, Haskell, Elm, Ruby and more."
        , feedAuthorName  = "Robert Pearce"
        , feedAuthorEmail = "me@robertwpearce.com"
        , feedRoot        = root
        }


-- CUSTOM ROUTE


getSlugWords :: Text -> [Text]
getSlugWords =
    T.words . T.toLower . T.map f . T.replace "'" "" . T.replace "&" "and"
    where
        f x = if isAlphaNum x then x else ' '


getSlug :: String -> String
getSlug =
    T.unpack . T.intercalate (T.singleton '-') . getSlugWords . T.pack


getTitleFromMeta :: Metadata -> String
getTitleFromMeta =
    fromMaybe "no title" . lookupString "title"


fileNameFromTitle :: Metadata -> String
fileNameFromTitle =
    (++ ".html") . getSlug . getTitleFromMeta


customizedRoute :: Metadata -> Routes
customizedRoute =
    customRoute . const . fileNameFromTitle
