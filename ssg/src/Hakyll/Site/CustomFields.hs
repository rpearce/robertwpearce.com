module Hakyll.Site.CustomFields (updatedField) where

--------------------------------------------------------------------------------

import qualified Control.Monad as Monad
import qualified Control.Monad.Fail as MonadFail
import qualified Data.Maybe as Maybe
import qualified Data.Time.Clock as DTC
import qualified Data.Time.Format as DTF
import qualified Data.Time.Locale.Compat as DTLC
import qualified Hakyll as H

--------------------------------------------------------------------------------

updatedField :: String -> String -> H.Context String
updatedField key format = H.field key $ \i -> do
  let locale = DTLC.defaultTimeLocale
  time <- getUpdatedUTC locale $ H.itemIdentifier i
  return $ DTF.formatTime locale format time

--------------------------------------------------------------------------------

getUpdatedUTC :: (H.MonadMetadata m, MonadFail m)
              => DTLC.TimeLocale
              -> H.Identifier
              -> m DTC.UTCTime
getUpdatedUTC locale id' = do
  metadata <- H.getMetadata id'
  let tryField k fmt = H.lookupString k metadata >>= parseTime' fmt
  Maybe.maybe empty' return $ Monad.msum [tryField "updated" fmt | fmt <- formats]
  where
    empty'     = MonadFail.fail $ "getUpdatedUTC: " ++ "could not parse time for " ++ show id'
    parseTime' = DTF.parseTimeM True locale
    formats    =
      [ "%Y-%m-%d"
      , "%Y-%m-%dT%H:%M:%SZ" -- feed-friendly
      , "%Y-%m-%d @ %H:%M %Z" -- custom for notes
      ]
