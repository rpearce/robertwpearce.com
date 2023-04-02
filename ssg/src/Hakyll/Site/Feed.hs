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

-- H.renderRss: https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L163
createRss :: H.Rules ()
createRss = do
  H.route   H.idRoute
  H.compile $ feedCompiler H.renderRss

-- H.renderAtom: https://github.com/jaspervdj/hakyll/blob/66ace430f90ec97cbb9cf278ec46aec3b457fc56/lib/Hakyll/Web/Feed.hs#L172
createAtom :: H.Rules ()
createAtom = do
  H.route   H.idRoute
  H.compile $ feedCompiler H.renderAtom

--------------------------------------------------------------------------------

type FeedRenderer =
     H.FeedConfiguration
  -> H.Context String
  -> [H.Item String]
  ->  H.Compiler (H.Item String)

feedCompiler :: FeedRenderer -> H.Compiler (H.Item String)
feedCompiler renderer =
  renderer HSConfig.feedConfiguration feedCtx
    =<< H.recentFirst
    =<< H.loadAllSnapshots "posts/*" "content"

feedCtx :: H.Context String
feedCtx =
  titleCtx
    <> HSCustomFields.updatedField "updated" "%Y-%m-%dT%H:%M:%SZ"
    <> HSPost.postCtx
    <> H.bodyField "description"

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
