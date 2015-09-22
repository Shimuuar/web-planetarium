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
drawSky (Just pl) (Camera { cameraViewEq     = cam
                          , cameraViewHor    = camHor
                          , cameraZoom       = zoom
                          , cameraViewport   = (w,h)
                          , cameraProjection = prj
                          }) = do
  -- Start drawing
  clear
  let projector = case prj of
        ProjOrthographic -> orthographic
        ProjSterographic -> stereographic
      proj  = project projector . toCoord cam
      projH = project projector . toCoord camHor
      zoomFactor = zoom * fromIntegral (min w h) / 2.2
      scale p = let (x,y) = F.convert p
                    ss = zoomFactor
                    xx = ss * x + fromIntegral w / 2
                    yy = fromIntegral h / 2 - ss * y
                in (xx,yy)
  -- Draw sky
  fillStyle "#002"
  case maxR projector of
    Just r -> do
      arc (fromIntegral w/2, fromIntegral h/2) (r*zoomFactor) (0,2*pi)
      fill
    Nothing -> do
      fillRect 0 0 (fromIntegral w) (fromIntegral h)
  -- Draw grid
  beginPath
  lineWidth 0.5
  strokeStyle "#666"
  duration "grid" $ forM_ (coordGridEq pl) $ \ln -> do
    drawprojLine (fmap scale . proj) ln
  stroke
  -- Draw grid
  beginPath
  lineWidth 0.5
  strokeStyle "#585"
  duration "grid" $ forM_ (coordGridHor pl) $ \ln -> do
    drawprojLine (fmap scale . projH) ln
  stroke
  -- Draw constellation lines
  beginPath
  lineWidth 1
  strokeStyle "#558"
  duration "cline" $ forM_ (clines pl) $ \(CLines cl) -> do
    forM_ cl $ \contour ->
      case sequence $ proj <$> contour of
        Nothing -> return ()
        Just (fmap scale -> l) -> drawLine l
  stroke
  -- Draw stars
  beginPath
  fillStyle "#fff"
  duration "stars" $ forM_ (brightStars pl) $ \(p,m) ->
    forM_ (fmap scale $ proj p) $ \(x,y) -> do
      let r = (6.5 - m) / 1.8
      moveTo x y
      arc (x,y) r (0,2*pi)
  fill
  -- -- Draw center
  -- beginPath
  -- fillStyle "#f00"
  -- fillRect (fromIntegral w/2) (fromIntegral h/2) 2 2
