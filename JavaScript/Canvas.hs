{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
-- |
module JavaScript.Canvas (
    -- * Canvas accessors
    Canvas
  , runCanvas
  , width
  , height
  , resize
    -- * Drawing
  , fillRect
  , clearRect
  , moveTo
  , lineTo
  , stroke
  , strokeStyle
  , fill
  , fillStyle
  , beginPath
  , drawLine
  , drawprojLine
  , lineWidth
  , clear
  , arc
  ) where

import Control.Applicative
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import qualified Data.Foldable as F
import GHCJS.Types



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

----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

fillRect :: Double -> Double -> Double -> Double -> Canvas ()
fillRect x y w h = Canvas $ ReaderT $ \(_,cxt) ->
  jscnv_fillRect cxt x y w h

clearRect :: Double -> Double -> Double -> Double -> Canvas ()
clearRect x y w h = Canvas $ ReaderT $ \(_,cxt) ->
  jscnv_clearRect cxt x y w h

clear :: Canvas ()
clear = Canvas $ ReaderT $ \(cnv,_) -> jscnv_clear cnv

lineTo :: Double -> Double -> Canvas ()
lineTo x y = Canvas $ ReaderT $ \(_,cxt) -> jscnv_lineTo cxt x y

moveTo :: Double -> Double -> Canvas ()
moveTo x y = Canvas $ ReaderT $ \(_,cxt) -> jscnv_moveTo cxt x y

lineWidth :: Double -> Canvas ()
lineWidth w = Canvas $ ReaderT $ \(_,cxt) -> jscnv_lineWidth cxt w

stroke :: Canvas ()
stroke = Canvas $ ReaderT $ \(_,cxt) -> jscnv_stroke cxt 

fill :: Canvas ()
fill = Canvas $ ReaderT $ \(_,cxt) -> jscnv_fill cxt 

strokeStyle :: JSString -> Canvas ()
strokeStyle s = Canvas $ ReaderT $ \(_,cxt) -> jscnv_strokeStyle cxt s

fillStyle :: JSString -> Canvas ()
fillStyle s = Canvas $ ReaderT $ \(_,cxt) -> jscnv_fillStyle cxt s


resize :: Int -> Int -> Canvas ()
resize x y = Canvas $ ReaderT $ \(cnv,_) -> jscnv_resize cnv x y

width :: Canvas Int
width = Canvas $ ReaderT $ \(cnv,_) -> jscnv_width cnv

height :: Canvas Int
height = Canvas $ ReaderT $ \(cnv,_) -> jscnv_height cnv

beginPath :: Canvas ()
beginPath = Canvas $ ReaderT $ \(_,cxt) -> jscnv_beginPath cxt

arc :: (Double,Double) -> Double -> (Double,Double) -> Canvas ()
arc (x,y) r (a,b) = Canvas $ ReaderT $ \(_,cxt) ->
  jscnv_arc cxt x y r a b


----------------------------------------------------------------
-- Compound functions
----------------------------------------------------------------

drawLine :: F.Foldable f => f (Double,Double) -> Canvas ()
drawLine xs = case F.toList xs of
  []         -> return ()
  [_]        -> return ()
  ((x,y):ps) -> moveTo x y >> F.forM_ ps (uncurry lineTo)

drawprojLine :: F.Foldable f => (a -> Maybe (Double,Double)) -> f a -> Canvas ()
drawprojLine proj
  = mapM_ (\x -> drawLine x) . split . fmap proj . F.toList
  where
    split [] = []
    split xs = case spanJust xs of
      (a,rest) -> a : split rest
    --
    spanJust (Just a:xs) = case spanJust xs of
      (as,rest) -> (a:as,rest)
    spanJust xs = ([],dropNothing xs)
    --
    dropNothing (Nothing:xs) = dropNothing xs
    dropNothing xs = xs

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

foreign import javascript safe "$1.moveTo($2,$3)"
  jscnv_moveTo :: JSRef Context -> Double -> Double -> IO ()

foreign import javascript safe "$1.lineTo($2,$3)"
  jscnv_lineTo :: JSRef Context -> Double -> Double -> IO ()

foreign import javascript safe "$1.lineWidth = $2"
  jscnv_lineWidth :: JSRef Context -> Double -> IO ()

foreign import javascript safe "$1.stroke()"
  jscnv_stroke :: JSRef Context -> IO ()

foreign import javascript safe "$1.strokeStyle = $2"
  jscnv_strokeStyle :: JSRef Context -> JSString -> IO ()

foreign import javascript safe "$1.fill()"
  jscnv_fill :: JSRef Context -> IO ()

foreign import javascript safe "$1.fillStyle = $2"
  jscnv_fillStyle :: JSRef Context -> JSString -> IO ()

foreign import javascript safe "$1.width = $1.width"
  jscnv_clear :: JSRef CanvasTag -> IO ()

foreign import javascript safe "$1.width"
  jscnv_width :: JSRef CanvasTag -> IO Int

foreign import javascript safe "$1.height"
  jscnv_height :: JSRef CanvasTag -> IO Int

foreign import javascript safe "{$1.width = $2; $1.height = $3;}"
  jscnv_resize :: JSRef CanvasTag -> Int -> Int -> IO ()

foreign import javascript safe "$1.beginPath()"
  jscnv_beginPath :: JSRef Context -> IO ()

foreign import javascript safe "$1.arc($2,$3,$4,$5,$6)"
  jscnv_arc :: JSRef Context -> Double -> Double -> Double -> Double -> Double -> IO ()
