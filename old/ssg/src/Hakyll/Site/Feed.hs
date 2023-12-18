{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Hakyll.Site.Feed (createRss, createAtom) where

--------------------------------------------------------------------------------

import qualified Data.Maybe as Maybe
import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.CustomFields as HSCustomFields
import qualified Hakyll.Site.Post as HSPost

--------------------------------------------------------------------------------

createRss :: H.Rules ()
createRss = do
  H.route   H.idRoute
  H.compile $ compileRss

createAtom :: H.Rules ()
createAtom = do
  H.route   H.idRoute
  H.compile $ compileAtom

--------------------------------------------------------------------------------

-- H.renderRss: https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L163
compileRss :: H.Compiler (H.Item String)
compileRss =
  H.renderRss HSConfig.feedConfiguration rssCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "posts/*" "content"

-- H.renderAtom: https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L172
compileAtom :: H.Compiler (H.Item String)
compileAtom =
  H.renderAtom HSConfig.feedConfiguration atomCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "posts/*" "content"

--------------------------------------------------------------------------------

rssCtx :: H.Context String
rssCtx =
  HSCustomFields.updatedField "updated" "%a, %d %b %Y %H:%M:%S UT"
    <> H.field "title" updatedTitle
    <> HSPost.postCtx
    <> H.bodyField "description"

atomCtx :: H.Context String
atomCtx =
  HSCustomFields.updatedField "updated" "%Y-%m-%dT%H:%M:%SZ"
    <> H.field "title" updatedTitle
    <> HSPost.postCtx
    <> H.bodyField "description"

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
