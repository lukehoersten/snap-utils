module Snap.Utils
       ( module Snap.Utils.Alert
         -- $alerts
       , module Snap.Utils.Environment
         -- $environment
       , module Snap.Utils.ErrorLogger
         -- $errorlogger
       , module Snap.Utils.TargetPage
         -- $targetpage
       ) where

import           Snap.Utils.Alert
import           Snap.Utils.Environment
import           Snap.Utils.ErrorLogger
import           Snap.Utils.TargetPage

-- $alerts
--
-- 'Alerts' uses GET request URI query parameters to display an alert on the
-- next response page.

-- $environment
--
-- 'Environment' splices the @-e@ runtime environment (ex: devel,
-- prod, etc.) name into a template tag. This allows for splicing
-- certain content only when in a specific runtime environment.

-- $errorlogger
--
-- 'ErrorLogger' catches exceptions that are thrown all the way to the
-- top of the site and handles them cleanly without crashing the
-- application server.

-- $targetpage
--
-- 'TargetPage' captures the desired target page when interim pages
-- need to be visited first before continuing onto the target page
-- afterwards.
