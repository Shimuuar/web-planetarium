{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- FRP helpers for writing code
module Web.FRP (
    -- * Helpers
    consoleLog
    -- * FRP
  , runNowMaster'
  , actimate
  , actimateB
  , onClickStream
  , innerSizeBehavior
  ) where

import Control.Applicative
import Control.Monad
import Control.FRPNow
import Control.Concurrent (threadDelay)

import Data.String
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

----------------------------------------------------------------
-- High level functions
----------------------------------------------------------------

consoleLog :: String -> IO ()
consoleLog = js_console_log . fromString

-- | Execute Now computation
--
--   'runNowMaster' doesn't work
runNowMaster' :: Now () -> IO a
runNowMaster' now = do
  initNow void (now >> return never)
  forever $ threadDelay maxBound

actimate :: (Eq a) => (a -> IO ()) -> Behavior a -> Now ()
actimate fun bhv = do
  sync . fun =<< sample bhv
  callIOStream fun $ toChanges bhv

actimateB :: (Eq a) => (b -> a -> IO ()) -> Behavior b -> Behavior a -> Now ()
actimateB fun bhvB bhv = do
  b <- sample bhvB
  sync . fun b =<< sample bhv
  callIOStream id $ (fun <$> bhvB) <@@> toChanges bhv

-- | Stream of onclick events
onClickStream
  :: JSString                   -- ^ JQuery selector
  -> Now (EvStream ())
onClickStream selector = do
  -- Create event stream
  (stream,call) <- callbackStream
  -- Bind callback to JS
  jsCall <- sync $ syncCallback NeverRetain True $ call ()
  sync $ jq_event_click selector jsCall
  -- Done
  return stream

-- | Behaviour of value
innerSizeBehavior
  :: JSString                   -- ^ JQuery selector
  -> Now (Behavior (Int,Int))
innerSizeBehavior selector = do
  (stream,call) <- callbackStream
  let size = do
        h <- jq_innerHeight selector
        w <- jq_innerWidth selector
        return (w,h)
  jsCall <- sync $ syncCallback NeverRetain True $ call =<< size
  wh     <- sync size
  sync $ jq_event_resize jsCall
  sample $ foldrSwitch (pure wh) (pure <$> stream)


----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

foreign import javascript safe "$( $1 ).click( $2 )"
  jq_event_click :: JSString -> JSFun (IO ()) -> IO ()

foreign import javascript safe "$(window).resize( $1 )"
  jq_event_resize :: JSFun (IO ()) -> IO ()

foreign import javascript safe "$( $1 ).innerHeight()"
  jq_innerHeight :: JSString -> IO Int
foreign import javascript safe "$( $1 ).innerWidth()"
  jq_innerWidth :: JSString -> IO Int



foreign import javascript safe "console.log($1)"
  js_console_log :: JSString -> IO ()
