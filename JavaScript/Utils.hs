{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module JavaScript.Utils (
    -- * Javascript
    consoleLog
  , duration
    -- * Concurrency
  , MSink
  , newMSink
  , putMSink
  , takeMSink
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal


----------------------------------------------------------------
-- JS helpers
----------------------------------------------------------------

consoleLog :: String -> IO ()
consoleLog = js_console_log . toJSString

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
