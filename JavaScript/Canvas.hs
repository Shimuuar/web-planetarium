{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
module JavaScript.Canvas (
    Canvas
  , runCanvas
  , width
  , height
  , fillRect
  , clearRect
  , clear
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.FRPNow

import Data.Aeson
import Data.String
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

----------------------------------------------------------------
-- Canvas API
----------------------------------------------------------------

-- | Monad for drawing in canvas
newtype Canvas a = Canvas (ReaderT (JSRef CanvasTag,JSRef Context) IO a)
                   deriving (Functor,Applicative,Monad,MonadIO)

-- | Run context 
runCanvas :: JSString -> Canvas a -> IO a
runCanvas cnvId (Canvas m) = do
  cnv <- jscnv_getElementByID cnvId
  cxt <- jscnv_getContext cnv
  runReaderT m (cnv,cxt)

fillRect :: Double -> Double -> Double -> Double -> Canvas ()
fillRect x y w h = Canvas $ ReaderT $ \(_,cxt) ->
  jscnv_fillRect cxt x y w h

clearRect :: Double -> Double -> Double -> Double -> Canvas ()
clearRect x y w h = Canvas $ ReaderT $ \(_,cxt) ->
  jscnv_clearRect cxt x y w h

clear :: Canvas ()
clear = do 
  w <- width
  h <- height
  clearRect 0 0 (fromIntegral w) (fromIntegral h)
  
width :: Canvas Int
width = Canvas $ ReaderT $ \(cnv,_) -> jscnv_width cnv

height :: Canvas Int
height = Canvas $ ReaderT $ \(cnv,_) -> jscnv_width cnv


----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

data Context
data CanvasTag

foreign import javascript safe "document.getElementById($1)"
  jscnv_getElementByID :: JSString -> IO (JSRef CanvasTag)
  
foreign import javascript safe "$1.getContext('2d')"
  jscnv_getContext :: JSRef CanvasTag -> IO (JSRef Context)

foreign import javascript safe "$1.fillRect($2,$3,$4,$5)"
  jscnv_fillRect :: JSRef Context -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript safe "$1.clearRect($2,$3,$4,$5)"
  jscnv_clearRect :: JSRef Context -> Double -> Double -> Double -> Double -> IO ()

foreign import javascript safe "$1.width"
  jscnv_width :: JSRef CanvasTag -> IO Int

foreign import javascript safe "$1.height"
  jscnv_height :: JSRef CanvasTag -> IO Int
