{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}

import Control.Applicative
import Control.Monad
import Control.FRPNow
import Control.Concurrent
import Data.Angle
import Data.Quaternion
import Data.Monoid
import Data.String
import Data.Typeable
import qualified Data.Vector.Fixed as F
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import Celestial.Projection
import Celestial.Coordinates

import Web.FRP
import Web.JQ
import JavaScript.Canvas



----------------------------------------------------------------
-- JS

foreign import javascript safe "resize_canvas($1,$2)"
  js_resize_canvas :: Int -> Int -> IO ()

foreign import javascript safe "draw_point($1,$2)"
  js_draw_point :: Double -> Double -> IO ()

foreign import javascript safe "clear_canvas()"
  js_clear_canvas :: IO ()

foreign import javascript safe "{$($1).empty(); $($1).append($2);}"
  js_set_label :: JSString -> Double -> IO ()

----------------------------------------------------------------
-- Go!


adjustStream
  :: Num a
  => (JSString, a -> a)    -- ^ Decrement
  -> (JSString, a -> a)    -- ^ Increent
  -> Now (EvStream (a -> a))
adjustStream (down,funD) (up,funU) = do
  streamUp   <- onClickStream up
  streamDown <- onClickStream down
  return $ (funU <$ streamUp)
        <> (funD <$ streamDown)



main :: IO ()
main = runNowMaster' $ do
  -- ea <- fetchJSON "data/HD1.json" :: Now (Event [(Int,Float,Float)])
  -- planNow $ (sync . consoleLog . show) <$> ea
  ----------------------------------------------------------------
  -- Right-left
  streamUD <- adjustStream ("#btn-down", subtract 10) ("#btn-up",(+10))
  bhvUD    <- sample $ foldEs (\n f -> min 90 $ max (-90) $ f n) 0 streamUD
  -- Left-right
  streamLR <- adjustStream ("#btn-left",subtract 10) ("#btn-right",(+10))
  bhvLR    <- sample $ foldEs (\n f -> f n) 0 streamLR
  -- Zoom
  streamZ <- adjustStream ("#btn-zoomout", (/1.1)) ("#btn-zoomin",(*1.1))
  bhvZoom <- sample $ foldEs (\n f -> f n) 1 streamZ
  --
  actimate (js_set_label "#lab-delta") $ bhvUD
  actimate (js_set_label "#lab-alpha") $ bhvLR
  actimate (js_set_label "#lab-zoom")  $ bhvZoom

  ----------------------------------------------------------------
  let makeCamera a d =
        let α = Angle a :: Angle Degrees Double
            δ = Angle d :: Angle Degrees Double
        in 1
           * rotX (3*pi/2)
           * rotZ (pi/2)
           * rotY (asRadians δ)
           * rotZ (asRadians α) :: Quaternion Double
  -- let cameraQ = ((α,δ) -> 
  ss <- innerSizeBehavior "#area"
  flip actimate ss $ \(w,h) -> do
    js_resize_canvas w h
  
  -- actimate (consoleLog . show) (makeCamera <$> bhvLR <*> bhvUD)
  
-- Draw square
  let draw zoom (w,h) cam = runCanvas "cnv" $ do
        clear
        forM_ ([-75, -65 .. 85 ] ++ [90]) $ \δ ->
          forM_ ([0, 10 .. 270] ++ [1 .. 9]) $ \α -> do
            let a,d :: Angle Degrees Double
                a = Angle α
                d = Angle δ
            case fromSperical a d of
              Spherical v -> case project orthographic $ Spherical $ rotateVector cam v of
                Just (ProjCoord (F.convert -> (x,y))) -> do
                  let ss = zoom * fromIntegral (min w h) / 2.2
                      xx = ss * x + fromIntegral w / 2
                      yy = fromIntegral h / 2 - ss * y
                  fillRect xx yy 1 1
                Nothing -> return ()

  actimate (\(a,b,c) -> draw a b c)
    ((,,) <$> bhvZoom
          <*> ss
          <*> (makeCamera <$> bhvLR <*> bhvUD))
