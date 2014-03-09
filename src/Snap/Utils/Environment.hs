{-# LANGUAGE OverloadedStrings #-}

{-|

When starting a Snap app, the @-e@ command line flag indicates what
runtime environment to run in. The default is @devel@ which will load
all the @devel.cfg@ Snaplet config files. The 'Environment' module
adds a splice to conditionally render a node list depending on the
string passed to @-e@ on the command line.

Here's an example:

> initApp :: SnapletInit App App
> initApp = makeSnaplet "app" "An snaplet example application." Nothing $ do
>     h <- nestSnaplet "heist" heist $ heistInit "templates"
>     addEnvironmentSplices h
>     return $ App h

Then, in the Heist templates:

> <ifEnvironment name="devel">
>   <link href="static/css/bootstrap.min.css" rel="stylesheet">
>   <link href="static/css/font-awesome.min.css" rel="stylesheet">
> </ifEnvironment>
> <ifEnvironment name="prod">
>   <link href="http://cdn/css/bootstrap.min.css" rel="stylesheet">
>   <link href="http://cdn/css/font-awesome.min.css" rel="stylesheet">
> </ifEnvironment>

The @\<ifEnvironment name=\"prod\"\>@ block will only render if the
command line was passed @-e prod@.

-}

module Snap.Utils.Environment
       ( addEnvironmentSplices
       ) where

import           Control.Applicative ((<$>))
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         (mempty)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Heist               (HeistConfig (..), getParamNode)
import           Heist.Compiled      (codeGen, runChildren, yieldRuntime)
import           Heist.SpliceAPI     (( #! ))
import           Snap.Snaplet        (Initializer, Snaplet, getEnvironment)
import           Snap.Snaplet.Heist  (HasHeist, Heist, SnapletCSplice,
                                      SnapletISplice, addConfig)
import           Text.XmlHtml        (Node (..), childNodes)


-- | Add @\<ifEnvironment name=\"env\"\>@ splices to Heist state.
addEnvironmentSplices :: HasHeist b => Snaplet (Heist b) -> Initializer b v ()
addEnvironmentSplices h = do
    env <- T.pack <$> getEnvironment
    addConfig h $ mempty
        { hcCompiledSplices    = "ifEnvironment" #! envCSplice env
        , hcInterpretedSplices = "ifEnvironment" #! envISplice env
        }


envISplice :: Text -> SnapletISplice b
envISplice env = do
    node <- getParamNode
    let env' = getAttribute node "name"
    return $ if env' == env then childNodes node else []


envCSplice :: Text -> SnapletCSplice b
envCSplice env = do
    node <- getParamNode
    let env' = getAttribute node "name"
    children <- runChildren
    return . yieldRuntime $ if env' == env then codeGen children else mempty


getAttribute :: Node -> Text -> Text
getAttribute node attr =
    case node of
        Element _ as _ ->
            fromMaybe (error $ show node ++ ": missing " ++ T.unpack attr) $
            lookup attr as
        _ -> error "Wrong type of node!"
