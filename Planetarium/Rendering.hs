{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
-- |
-- Rendering of planetarium data
module Planetarium.Rendering where

import Control.Applicative
import Control.Monad hiding (forM_,sequence)
import Data.Foldable    (forM_)
import Data.Traversable (sequence)
import qualified Data.Vector.Fixed as F
import Data.Quaternion

import JavaScript.Canvas
import JavaScript.Utils
import Celestial.Projection
import Celestial.Coordinates
import Planetarium.Planetarium
import Planetarium.Camera
import Planetarium.Catalogs

import Prelude hiding (sequence)


----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Render planetarium on canvas
drawSky :: Maybe Planetarium -> Camera -> Canvas ()
drawSky Nothing _ = return ()
drawSky (Just pl) (Camera { cameraViewEq   = CoordTransform cam
                          , cameraViewHor  = CoordTransform camHor
                          , cameraZoom     = zoom
                          , cameraViewport = (w,h)
                          }) = do
  -- Start drawing
  clear
  let proj  (Spherical v) = (project orthographic . Spherical . rotateVector cam)    v
      projH (Spherical v) = (project orthographic . Spherical . rotateVector camHor) v
      zoomFactor = zoom * fromIntegral (min w h) / 2.2
      scale p = let (x,y) = F.convert p
                    ss = zoomFactor
                    xx = ss * x + fromIntegral w / 2
                    yy = fromIntegral h / 2 - ss * y
                in (xx,yy)
  -- Draw sky
  fillStyle "#aaf"
  arc (fromIntegral w/2, fromIntegral h/2) zoomFactor (0,2*pi)
  fill
  -- Draw grid
  beginPath
  lineWidth 0.5
  strokeStyle "#ccf"
  duration "grid" $ forM_ (coordGridEq pl) $ \ln -> do
    drawprojLine (fmap scale . proj) ln
  stroke
  -- Draw grid
  beginPath
  lineWidth 0.5
  strokeStyle "#cfc"
  duration "grid" $ forM_ (coordGridHor pl) $ \ln -> do
    drawprojLine (fmap scale . projH) ln
  stroke
  -- Draw constellation lines
  beginPath
  lineWidth 1
  strokeStyle "#448"
  duration "cline" $ forM_ (clines pl) $ \(CLines cl) -> do
    forM_ cl $ \contour ->
      case sequence $ proj <$> contour of
        Nothing -> return ()
        Just (fmap scale -> l) -> drawLine l
  stroke
  -- Draw stars
  beginPath
  fillStyle "#fff"
  forM_ (brightStars pl) $ \(p,m) ->
    forM_ (fmap scale $ proj p) $ \(x,y) -> do
      let r = (6 - m) / 1.5
      moveTo x y
      arc (x,y) r (0,2*pi)
  fill
