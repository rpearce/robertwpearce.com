{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import qualified Hakyll as H
import qualified Hakyll.Site.Assets as HSAssets
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.Feed as HSFeed
import qualified Hakyll.Site.Rules as HSRules
import qualified Hakyll.Site.Sitemap as HSSitemap

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Fingerprint the static assets up front so rendered pages can cache-bust
  -- their URLs with `?v=<hash>` (see Hakyll.Site.Assets). No generated assets
  -- here: the chroma highlight styles live in the regular css/ files.
  manifest <-
    HSAssets.buildManifest
      (H.providerDirectory HSConfig.hakyllConfiguration)
      []

  H.hakyllWith HSConfig.hakyllConfiguration $ do
    -- COPY FILES
    H.match ".well-known/*" HSRules.copy
    H.match "examples/**" HSRules.copy
    H.match "fonts/*" HSRules.copy
    H.match "images/*" HSRules.copy
    H.match "pdfs/*" HSRules.copy
    H.match "robots.txt" HSRules.copy

    -- BUILD CSS
    H.match "css/*" HSRules.css

    -- BUILD PAGES
    H.match "templates/*" HSRules.templates
    H.match "posts/*" (HSRules.posts manifest)
    H.match "new-zealand/**" (HSRules.nz manifest)
    H.match "index.html" (HSRules.index manifest)

    -- BUILD META
    H.create ["sitemap.xml"] HSSitemap.createSitemap
    H.create ["rss.xml"] HSFeed.createRss
    H.create ["atom.xml"] HSFeed.createAtom
