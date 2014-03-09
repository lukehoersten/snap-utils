# Snap Utils

## Overview

`Snap.Utils` provides helper modules for making web apps with Snap
Framework. The underlying design goal is to rely on more traditional
HTTP-based methods of stateless control flow instead of relying on
cookies and server state continuations.

See
[snap-utils Hackage package](http://hackage.haskell.org/package/snap-utils)
for more information.

## Modules

### Alerts

`Alerts` uses GET request URI query parameters to display an alert on
the next response page.

### Environment

`Environment` splices the `-e` runtime environment (ex: devel, prod,
etc.) name into a template tag. This allows for splicing certain
content only when in a specific runtime environment.

### Error Logger

`ErrorLogger` catches exceptions that are thrown all the way to the
top of the site and handles them cleanly without crashing the
application server.

### Target Page

`TargetPage` captures the desired target page when interim pages
need to be visited first before continuing onto the target page
afterwards.
