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
  => a                          -- ^ Inc/decrement size
  -> JSString                   -- ^ Decrement selector
  -> JSString                   -- ^ Increment selector
  -> Now (EvStream (a -> a))
adjustStream delta down up = do
  streamUp   <- onClickStream up
  streamDown <- onClickStream down
  return $ ((+delta)         <$ streamUp)
        <> ((subtract delta) <$ streamDown)



main :: IO ()
main = runNowMaster' $ do
  ea <- fetchJSON "data/HD1.json" :: Now (Event [(Int,Float,Float)])
  planNow $ (\a -> sync $ consoleLog (show a)) <$> ea

    
  
  ----------------------------------------------------------------
  -- Right-left
  streamUD <- adjustStream 10 "#btn-down" "#btn-up"
  bhvUD    <- sample $ foldEs (\n f -> min 90 $ max (-90) $ f n) 0 streamUD
  -- Left-right
  streamLR <- adjustStream 10 "#btn-left" "#btn-right"
  bhvLR    <- sample $ foldEs (\n f -> f n) 0 streamLR
  --
  actimate (js_set_label "#lab-delta") $ bhvUD
  actimate (js_set_label "#lab-alpha") $ bhvLR
  ----------------------------------------------------------------
  let makeCamera a d =
        let α = Angle a :: Angle Degrees Double
            δ = Angle d :: Angle Degrees Double
        in 1
           * rotX (3*pi/2)
           * rotZ (pi/2)
           * rotY (asRadians δ)
           * rotZ (asRadians α)
           :: Quaternion Double
  -- let cameraQ = ((α,δ) -> 
  ss <- innerSizeBehavior "#area"
  flip actimate ss $ \(w,h) -> do
    js_resize_canvas w h
  
  -- actimate (consoleLog . show) (makeCamera <$> bhvLR <*> bhvUD)
  
-- Draw square
  let draw cam = runCanvas "cnv" $ do
        clear
        forM_ ([-75, -65 .. 85 ] ++ [90]) $ \δ ->
          forM_ ([0, 10 .. 270] ++ [1 .. 9]) $ \α -> do
            let a,d :: Angle Degrees Double
                a = Angle α
                d = Angle δ
            case fromSperical a d of
              Spherical v -> case project orthographic $ Spherical $ rotateVector cam v of
                Just (ProjCoord (F.convert -> (x,y))) -> fillRect (180*x + 200) (200 - 180*y) 1 1
                Nothing -> return ()

  actimate draw (makeCamera <$> bhvLR <*> bhvUD)
  -- let bhv = (,) <$> bhvLR <*> bhvUD
  -- x <- sample bhv
  -- sync $ draw x
  -- callIOStream draw $ toChanges bhv
  -- return ()
