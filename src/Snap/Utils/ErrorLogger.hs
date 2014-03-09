{-# LANGUAGE OverloadedStrings #-}

module Snap.Utils.ErrorLogger where

import           Control.Exception     (SomeException)
import           Control.Monad.CatchIO (catch)
import           Data.Maybe            (fromJust)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Snap.Core             (MonadSnap, getRequest, logError)
import           Snap.Snaplet          (Handler, SnapletLens, with)
import           Snap.Snaplet.Auth     (AuthManager, AuthUser, currentUser,
                                        unUid, userId, userLogin)
import           Snap.Utils.Alert      (alertDanger)
import           Snap.Utils.Types      (URL)


newtype SysMsg = SysMsg { unSysMsg :: Text } deriving (Show, Eq)
newtype UsrMsg = UsrMsg { unUsrMsg :: Text } deriving (Show, Eq)


topExceptionHandler :: SnapletLens v (AuthManager b) -> Handler b v () -> Handler b v ()
topExceptionHandler auth = flip catch (logException auth)


logException :: SnapletLens v (AuthManager b) -> SomeException -> Handler b v ()
logException auth e = do
  rq <- getRequest
  logErrorRedirect (sysMsg rq) usrMsg url =<< (with auth currentUser)
    where sysMsg rq = SysMsg $ T.concat ["Uncaught exception: '" , T.pack $ show e, "'\n", T.pack $ show rq]
          usrMsg = UsrMsg "You hit an application problem."
          url = "/"

-- Log a system error and redirect the user with a nice message.
-- TODO log JSON
logErrorRedirect :: MonadSnap m => SysMsg -> UsrMsg -> URL -> Maybe AuthUser -> m a
logErrorRedirect sysMsg usrMsg url user = do
  logError . T.encodeUtf8 $ T.concat ["[User: ", maybe "UNKNOWN" tShowUser user, "] ", unSysMsg sysMsg]
  alertDanger usrMsg' url
      where tShowUser u = T.concat [unUid . fromJust $ userId u, " ", userLogin u]
            usrMsg' = T.concat [unUsrMsg usrMsg, " We've been notified of the problem and are working on it."]


logErrorRedirect' :: MonadSnap m => Text -> URL -> AuthUser -> m a
logErrorRedirect' msg url user = logErrorRedirect (SysMsg msg) (UsrMsg msg) url (Just user)
