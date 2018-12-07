{-# LANGUAGE OverloadedStrings #-}


import           Hakyll

import           Control.Monad (forM_)
import           Data.Char     (isAlphaNum)
import           Data.Maybe    (fromMaybe)
import qualified Data.Text     as T
import           Text.Pandoc
import           Web.Slug      (mkSlug)


main :: IO ()
main = hakyllWith config $ do
    forM_ [ "CNAME"
          , "robots.txt"
          , "_config.yml"
          , "images/*"
          , "fonts/*"
          , ".well-known/*"
          ] $ \f -> match f $ do
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


    match "new-zealand/**" $ do
        let ctx = constField "type" "article" <> postCtx

        route $ setExtension "html"
        compile $ pandocCompilerCustom
            >>= loadAndApplyTemplate "templates/info.html"    ctx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" ctx


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "root" root                   <>
                    constField "siteName" siteName           <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx


    match "templates/*" $ compile templateBodyCompiler


    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            nzPages <- loadAll "new-zealand/**"
            let pages = posts <> nzPages
                sitemapCtx =
                    constField "root" root         <>
                    constField "siteName" siteName <>
                    listField "pages" postCtx (return pages)
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx


    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = titleContext <> postCtx <> bodyField "description"
            posts <- recentFirst =<< loadAll "posts/*"
            renderRss feedConfiguration feedCtx posts


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


siteName :: String
siteName =
    "Robert Pearce | Freelance Software Developer"


config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "docs"
        , previewHost          = "127.0.0.1"
        , previewPort          = 8000
        , ignoreFile           = const False
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
    constField "root" root          <>
    constField "siteName" siteName  <>
    dateField "date" "%Y-%m-%d"     <>
    defaultContext


-- PANDOC


pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
    pandocCompilerWith pandocReaderOpts pandocWriterOpts


pandocExtensionsCustom :: Extensions
pandocExtensionsCustom =
    githubMarkdownExtensions <>
    extensionsFromList
        [ Ext_auto_identifiers
        , Ext_smart
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


getSlugWords :: T.Text -> [T.Text]
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
