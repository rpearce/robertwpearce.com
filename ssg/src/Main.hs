{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

import qualified Hakyll as H
import qualified Hakyll.Site.Configuration as HSConfig
import qualified Hakyll.Site.Feed as HSFeed
import qualified Hakyll.Site.Rules as HSRules
import qualified Hakyll.Site.Sitemap as HSSitemap

--------------------------------------------------------------------------------

main :: IO ()
main = H.hakyllWith HSConfig.hakyllConfiguration $ do
  -- COPY FILES
  H.match ".well-known/*" HSRules.copy
  H.match "CNAME" HSRules.copy
  H.match "_config.yml" HSRules.copy
  H.match "examples/**" HSRules.copy
  H.match "fonts/*" HSRules.copy
  H.match "images/*" HSRules.copy
  H.match "pdfs/*" HSRules.copy
  H.match "robots.txt" HSRules.copy

  -- BUILD CSS
  H.match "css/*" HSRules.css

  -- BUILD PAGES
  H.match "templates/*" HSRules.templates
  H.match "posts/*" HSRules.posts
  H.match "new-zealand/**" HSRules.nz
  H.match "index.html" HSRules.index

  -- BUILD META
  H.create ["sitemap.xml"] HSSitemap.createSitemap
  H.create ["rss.xml"] HSFeed.createRss
  H.create ["atom.xml"] HSFeed.createAtom
