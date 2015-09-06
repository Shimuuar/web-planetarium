{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- FRP helpers for writing code
module Web.JQ where

import Control.Applicative
import Control.Monad
import Control.FRPNow
import Control.Concurrent (threadDelay)

import Data.Aeson
import Data.String
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL


import Web.FRP

----------------------------------------------------------------
--
----------------------------------------------------------------

fetchJSON :: FromJSON a => String -> Now (Event (Maybe a))
fetchJSON url = do
  (ea,cb) <- callback
  jfun <- sync $ syncCallback1 NeverRetain True $ \val -> do
    Just a <- fromJSRef val -- =<< js_parse_json val
    -- let t  = fromJSString jss
    --     bs = T.encodeUtf8 t
    --     Just a = decode $ BL.fromStrict bs
    -- consoleLog $ show a
    cb $ case fromJSON a of { Success a -> Just a; _ -> Nothing}
  sync $ jq_fetch_json (toJSString url) jfun
  return ea

fetchText :: String -> Now (Event T.Text)
fetchText url = do
  (ea,cb) <- callback
  jfun <- sync $ syncCallback1 NeverRetain True $ \val -> do
    cb $ fromJSString val
  sync $ jq_fetch_text (toJSString url) jfun
  return ea



foreign import javascript safe "$.ajax({dataType:'json', url:$1, success:$2})"
  jq_fetch_json :: JSString -> JSFun (JSRef Value -> IO ()) -> IO ()

foreign import javascript safe "$.ajax({dataType:'text', url:$1, success:$2})"
  jq_fetch_text :: JSString -> JSFun (JSString -> IO ()) -> IO ()

foreign import javascript safe "JSON.parse($1)"
  js_parse_json :: JSString -> IO (JSRef Value)
