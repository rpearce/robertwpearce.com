{-# LANGUAGE OverloadedStrings #-}


import           Hakyll

import           Control.Monad    (forM_, msum)
import           Data.Maybe       (fromJust, fromMaybe)
import qualified Data.Text        as T
import qualified Data.Time.Clock  as Clock
import           Data.Time.Format (TimeLocale, defaultTimeLocale, formatTime,
                                   parseTimeM)
import           Slug             (toSlug)
import           Text.Pandoc      (Extension (Ext_auto_identifiers, Ext_fenced_code_attributes, Ext_smart),
                                   Extensions, ReaderOptions, WriterOptions,
                                   extensionsFromList, githubMarkdownExtensions,
                                   readerExtensions, writerExtensions)


main :: IO ()
main = hakyllWith config $ do
    forM_ [ "CNAME"
          , "favicon.ico"
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

        route $ metadataRoute titleRoute
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
        compile (feedCompiler renderRss)


    create ["atom.xml"] $ do
        route idRoute
        compile (feedCompiler renderAtom)


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


feedCtx :: Context String
feedCtx =
    titleCtx     <>
    updatedField <>
    postCtx      <>
    bodyField "description"


postCtx :: Context String
postCtx =
    constField "root" root         <>
    constField "siteName" siteName <>
    dateField "date" "%Y-%m-%d"    <>
    defaultContext


titleCtx :: Context String
titleCtx =
    field "title" updatedTitle


updatedField :: Context String
updatedField = field "updated" $ \i -> do
    let locale = defaultTimeLocale
    time <- getUpdatedUTC locale $ itemIdentifier i
    return $ formatTime locale "%Y-%m-%dT%H:%M:%SZ" time


-- ripped from https://github.com/jaspervdj/hakyll/blob/c85198d8cb6ce055c788e287c7f2470eac0aad36/lib/Hakyll/Web/Template/Context.hs#L296
getUpdatedUTC :: MonadMetadata m => TimeLocale -> Identifier -> m Clock.UTCTime
getUpdatedUTC locale id' = do
    metadata <- getMetadata id'
    let tryField k fmt = lookupString k metadata >>= parseTime' fmt
    maybe empty' return $ msum [tryField "updated" fmt | fmt <- formats]
  where
    empty'     = fail $ "Hakyll.Web.Template.Context.getUpdatedUTC: " ++ "could not parse time for " ++ show id'
    parseTime' = parseTimeM True locale
    formats    =
        [ "%a, %d %b %Y %H:%M:%S %Z"
        , "%Y-%m-%dT%H:%M:%S%Z"
        , "%Y-%m-%d %H:%M:%S%Z"
        , "%Y-%m-%d"
        , "%B %e, %Y %l:%M %p"
        , "%B %e, %Y"
        , "%b %d, %Y"
        ]


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


-- PANDOC


pandocCompilerCustom :: Compiler (Item String)
pandocCompilerCustom =
    pandocCompilerWith pandocReaderOpts pandocWriterOpts


pandocExtensionsCustom :: Extensions
pandocExtensionsCustom =
    githubMarkdownExtensions <>
    extensionsFromList
        [ Ext_auto_identifiers
        , Ext_fenced_code_attributes
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


type FeedRenderer =
    FeedConfiguration
    -> Context String
    -> [Item String]
    -> Compiler (Item String)


feedCompiler :: FeedRenderer -> Compiler (Item String)
feedCompiler renderer =
    renderer feedConfiguration feedCtx
        =<< recentFirst
        =<< loadAllSnapshots "posts/*" "content"


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


getTitleFromMeta :: Metadata -> String
getTitleFromMeta =
    fromMaybe "no title" . lookupString "title"


fileNameFromTitle :: Metadata -> FilePath
fileNameFromTitle =
    T.unpack . (`T.append` ".html") . toSlug . T.pack . getTitleFromMeta


titleRoute :: Metadata -> Routes
titleRoute =
    constRoute . fileNameFromTitle
