{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module JavaScript.Utils (
    -- * Javascript
    consoleLog
  , duration
    -- * FRP utils
  , fetchJSON
  , fetchText
    -- * Concurrency
  , MSink
  , newMSink
  , putMSink
  , takeMSink
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.FRPNow
import Data.Aeson
import qualified Data.Text as T
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal


----------------------------------------------------------------
-- JS helpers
----------------------------------------------------------------

-- | Write string to console
consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString

-- | Write duration of action to console
duration :: MonadIO m => String -> m a -> m a
duration msg io = do
  t1 <- liftIO js_date_now
  a  <- io
  t2 <- liftIO js_date_now
  liftIO $ consoleLog $ msg ++ ": " ++ show (t2-t1) ++ "ms"
  return a

foreign import javascript safe "console.log($1)"
  js_console_log :: JSString -> IO ()

foreign import javascript safe "Date.now()"
  js_date_now :: IO Double


----------------------------------------------------------------
-- FRP utils
----------------------------------------------------------------

-- | Fetch JSON object from URL
fetchJSON :: FromJSON a => String -> Now (Event (Maybe a))
fetchJSON url = do
  (ea,cb) <- callback
  jfun <- sync $ syncCallback1 NeverRetain True $ \val -> do
    Just a <- fromJSRef val
    cb $ case fromJSON a of
           Success a -> Just a
           _         -> Nothing
  sync $ jq_fetch_json (toJSString url) jfun
  return ea

-- | Fetch text from URL
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

----------------------------------------------------------------
-- Concurrency
----------------------------------------------------------------

-- | MVar variant with non-blocking put which overwrites current
-- content
data MSink a = MSink (MVar a) (MVar ())

-- | Create new empty msink
newMSink :: IO (MSink a)
newMSink = MSink <$> newEmptyMVar <*> newMVar ()

-- | Put value into sink
putMSink :: MSink a -> a -> IO ()
putMSink (MSink mv sem) a = do
  () <- takeMVar sem
  _  <- tryTakeMVar mv
  putMVar mv  a
  putMVar sem ()

-- | Take value from sink
takeMSink :: MSink a -> IO a
takeMSink (MSink mv _) = do
  takeMVar mv
