{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Snap.Utils.TargetPage where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans (lift)
import           Data.Monoid         (mempty)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Heist               (HeistConfig (..))
import           Heist.Compiled      (yieldRuntimeText)
import           Heist.Interpreted   (textSplice)
import           Heist.SpliceAPI     (( #! ))
import           Network.HTTP.Types  (urlDecode, urlEncode)
import           Snap.Core           (MonadSnap, getParam, getRequest, redirect,
                                      rqURI)
import           Snap.Snaplet        (Initializer, Snaplet)
import           Snap.Snaplet.Heist  (HasHeist, Heist, SnapletCSplice,
                                      SnapletISplice, addConfig)
import           Snap.Utils.Alert    (AlertType (..), alertRedirect')
import           Snap.Utils.Types    (URL)


-- |Defer the target-page
deferFromTargetPage :: MonadSnap m => Text -> URL -> m a
deferFromTargetPage msg interimUrl =
    deferFromTargetPage' msg interimUrl =<< rqURI <$> getRequest


-- |If redirecting from a POST action, the POST action wasn't the
-- target. The referrer was. Just pass the target-url in directly
-- instead of grabbing it from the request.
deferFromTargetPage' :: MonadSnap m => Text -> URL -> URL -> m a
deferFromTargetPage' msg interimUrl targetUrl =
    alertRedirect' Warning msg interimUrl . (:[]) . ("target-page",) $
                   urlEncode True targetUrl


continueToTargetPage :: MonadSnap m => m a -> m a
continueToTargetPage noTargetPage =
    getParam "target-page" >>= maybe noTargetPage hasTargetPage
        where hasTargetPage = redirect . urlDecode True


addTargetPageSplice :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
addTargetPageSplice h = addConfig h $ mempty
    { hcCompiledSplices    = ("target-page" #! targetPageCSplice)
    , hcInterpretedSplices = ("target-page" #! targetPageISplice)
    }


targetPageISplice :: SnapletISplice b
targetPageISplice = do
    targetPage <- getParam "target-page"
    textSplice $ maybe "" (T.append "target-page=" . T.decodeUtf8) targetPage


targetPageCSplice :: SnapletCSplice b
targetPageCSplice = return . yieldRuntimeText . lift $ do
    targetPage <- getParam "target-page"
    return $ maybe "" (T.append "target-page=" . T.decodeUtf8) targetPage
