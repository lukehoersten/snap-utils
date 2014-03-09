{-# LANGUAGE OverloadedStrings #-}

module Snap.Utils.Alert
  ( addAlertSplices
  , alertRedirect
  , alertRedirect'
  , alertSuccess
  , alertInfo
  , alertWarning
  , alertDanger
  , alertUrl
  , AlertType(..)
  ) where


import           Control.Applicative   ((<$>), (<*>))
import           Control.Arrow         ((***))
import           Control.Monad         (liftM2)
import           Control.Monad.Trans   (lift)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Monoid           (mempty)
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as T
import           Heist                 (HeistConfig (..), RuntimeSplice,
                                        Splices, mapS, noSplices)
import           Heist.Compiled        (Splice, callTemplate, codeGen,
                                        pureSplice, textSplice,
                                        withLocalSplices, yieldRuntime)
import           Heist.Interpreted     (callTemplateWithText)
import           Heist.SpliceAPI       (( #! ), ($$))
import           Snap.Core             (MonadSnap, getParam, redirect)
import           Snap.Snaplet          (Initializer, Snaplet)
import           Snap.Snaplet.Heist    (HasHeist, Heist, SnapletCSplice,
                                        SnapletISplice, addConfig)
import           Snap.Utils.Types      (URL)


data AlertType = Success | Info | Warning | Danger deriving Show


addAlertSplices :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
addAlertSplices h = addConfig h $ mempty
    { hcCompiledSplices    = ("alerts" #! alertCSplice)
    , hcInterpretedSplices = ("alerts" #! alertISplice)
    }


alertRedirect :: MonadSnap m => AlertType -> Text -> URL -> m a
alertRedirect typ msg url = alertRedirect' typ msg url []


alertRedirect' :: MonadSnap m => AlertType -> Text -> URL -> [(ByteString, ByteString)] -> m a
alertRedirect' typ msg url params = redirect $ alertUrl typ msg url params


alertSuccess :: MonadSnap m => Text -> URL -> m a
alertSuccess = alertRedirect Success


alertInfo :: MonadSnap m => Text -> URL -> m a
alertInfo = alertRedirect Info


alertWarning :: MonadSnap m => Text -> URL -> m a
alertWarning = alertRedirect Warning


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


alertUrl :: AlertType -> Text -> URL -> [(ByteString, ByteString)] -> URL
alertUrl typ msg url params = B.append url . uriQueryString $
  [("alert-text", T.encodeUtf8 msg), ("alert-type", cssType typ)] ++ params


cssType :: AlertType -> ByteString
cssType Success = "success"
cssType Info    = "info"
cssType Warning = "warning"
cssType Danger  = "danger"


uriQueryString :: [(ByteString, ByteString)] -> URL
uriQueryString = B.append "?" . B.intercalate "&" . map (showPairWith "=")
  where showPairWith sep (k, v) = B.concat [k, sep, v]
