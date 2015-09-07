{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
module JavaScript.Utils (
    consoleLog
  , duration  
  ) where

import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal


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
