{-# LANGUAGE OverloadedStrings #-}

{-|

There are two components to the @Alerts@ module.

    1. <http://hackage.haskell.org/package/heist Heist> splices for rendering the alerts

    2. Alert redirection helpers which pass the alert type and message
       via the HTTP GET query string.

First the Heist splices can be added with the 'addAlertSplices'
function which takes the Heist Snaplet as an argument.

> initApp :: SnapletInit App App
> initApp = makeSnaplet "app" "An snaplet example application." Nothing $ do
>     h <- nestSnaplet "heist" heist $ heistInit "templates"
>     addAlertSplices h
>     return $ App h

Second, generate an alert in a handler.

> import Snap.Utils.Alert (alertSuccess)
>
> actionSuccess :: Handler App App ()
> actionSuccess reg = alertSuccess msg url
>     where msg = "Successfully completed an action!"
>           url = "/"

Third, ensure the Heist template has a place to bind alerts.

> <alerts>
>  <div class="alerts">
>   <div class="fade in alert alert-${alert-type} alert-dismissable">
>     <button type="button" class="close" data-dismiss="alert">&times;</button>
>     <strong><alert-text/></strong>
>   </div>
>  </div>
> </alerts>

@Snap.Utils.Alert@ is different from similar modules because it relies
on more traditional HTTP-based methods of stateless control flow like
GET queries instead of relying on cookies and server state
continuations. <http://hackage.haskell.org/package/snap-extras/docs/Snap-Extras-FlashNotice.html Snap.Extras.FlashNotice>,
for example, uses cookies to store alert state.

-}

module Snap.Utils.Alert
  ( AlertType(..)
  , addAlertSplices
  , alertRedirect
  , alertRedirect'
  , alertSuccess
  , alertInfo
  , alertWarning
  , alertDanger
  , alertUrl
  ) where

import           Control.Applicative    ((<$>), (<*>))
import           Control.Arrow          ((***))
import           Control.Monad          (liftM2)
import           Control.Monad.Trans    (lift)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B
import           Data.Monoid            (mempty)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as T
import           Heist                  (HeistConfig (..), RuntimeSplice,
                                         Splices, mapS, noSplices)
import           Heist.Compiled         (Splice, callTemplate, codeGen,
                                         pureSplice, textSplice,
                                         withLocalSplices, yieldRuntime)
import           Heist.Interpreted      (callTemplateWithText)
import           Heist.SpliceAPI        (( #! ), ($$))
import           Network.HTTP.Types.URI (SimpleQuery, renderSimpleQuery)
import           Snap.Core              (MonadSnap, getParam, redirect)
import           Snap.Snaplet           (Initializer, Snaplet)
import           Snap.Snaplet.Heist     (HasHeist, Heist, SnapletCSplice,
                                         SnapletISplice, addConfig)
import           Snap.Utils.Types       (URL)


data AlertType = Success | Info | Warning | Danger deriving Show


-- | Add the compiled and interpreted alert splices to the
-- @\<alerts\>@ tag with nested tags of @\<alert-text\>@ and
-- @\<alert-type\>@ which will be bound to an 'AlertType'.
addAlertSplices :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
addAlertSplices h = addConfig h $ mempty
    { hcCompiledSplices    = ("alerts" #! alertCSplice)
    , hcInterpretedSplices = ("alerts" #! alertISplice)
    }


-- | 302 redirect to the target page URL with the specified
-- 'AlertType' and message.
alertRedirect :: MonadSnap m => AlertType -> Text -> URL -> m a
alertRedirect typ msg url = alertRedirect' typ msg url []


-- | Same as 'alertRedirect' but accepts additional query parameters.
alertRedirect' :: MonadSnap m => AlertType -> Text -> URL -> SimpleQuery -> m a
alertRedirect' typ msg url params = redirect $ alertUrl typ msg url params


-- | 'alertRedirect' with 'Success'
alertSuccess :: MonadSnap m => Text -> URL -> m a
alertSuccess = alertRedirect Success


-- | 'alertRedirect' with 'Info'
alertInfo :: MonadSnap m => Text -> URL -> m a
alertInfo = alertRedirect Info


-- | 'alertRedirect' with 'Warning'
alertWarning :: MonadSnap m => Text -> URL -> m a
alertWarning = alertRedirect Warning


-- | 'alertRedirect' with 'Danger'
alertDanger :: MonadSnap m => Text -> URL -> m a
alertDanger = alertRedirect Danger


alertISplice :: SnapletISplice b
alertISplice = do
  mAlert <- liftM2 (,) <$> getParam "alert-type" <*> getParam "alert-text"
  case mAlert of
    Just (aType, aText) ->
        callTemplateWithText "/snippet/alerts" $ do
              "alert-text" #! T.decodeUtf8 aText
              "alert-type" #! T.decodeUtf8 aType
    Nothing -> return []


alertCSplice :: SnapletCSplice b
alertCSplice = do
    children <- withLocalSplices (alertSplices $$ ss) noSplices (callTemplate "/snippet/alerts")
    return . yieldRuntime $ do
        mAlert <- lift $ liftM2 (,) <$> getParam "alert-type" <*> getParam "alert-text"
        maybe mempty (const $ codeGen children) mAlert
  where ss = do
            mAlert <- lift $ liftM2 (,) <$> getParam "alert-type" <*> getParam "alert-text"
            return $ maybe mempty (T.decodeUtf8 *** T.decodeUtf8) mAlert


alertSplices :: Monad m => Splices (RuntimeSplice m (Text, Text) -> Splice m)
alertSplices = mapS (pureSplice . textSplice) $ do
    "alert-type" #! fst
    "alert-text" #! snd


-- | Generate a URL with an alert query string without redirecting to
-- the URL.
alertUrl :: AlertType -> Text -> URL -> SimpleQuery -> URL
alertUrl typ msg url params = B.append url . renderSimpleQuery True $
  [("alert-text", T.encodeUtf8 msg), ("alert-type", cssType typ)] ++ params


cssType :: AlertType -> ByteString
cssType Success = "success"
cssType Info    = "info"
cssType Warning = "warning"
cssType Danger  = "danger"
