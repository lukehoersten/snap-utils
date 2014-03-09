{-# LANGUAGE OverloadedStrings #-}

module Snap.Utils.Environment where

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
