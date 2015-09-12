{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Applicative
import Control.Monad hiding (forM_,sequence)
import Control.Monad.IO.Class
import Control.FRPNow
import Control.Concurrent
import Data.Aeson
import Data.Angle
import Data.Quaternion
import Data.Monoid
import Data.String
import Data.Typeable
import qualified Data.Vector.Fixed as F
import qualified Data.Text as T
import Data.Foldable (forM_)
import Data.Traversable (sequence)
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import Celestial.Projection
import Celestial.Coordinates
import Celestial.Catalog

import Web.FRP
import Web.JQ
import JavaScript.Canvas
import JavaScript.Utils

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.HashMap.Strict as HM

import Prelude hiding (sequence)

import Planetarium.Catalogs
import Planetarium.Camera
import Planetarium.Planetarium
import Planetarium.Rendering

----------------------------------------------------------------
-- JS
----------------------------------------------------------------

foreign import javascript safe "{$($1).empty(); $($1).append(''+$2);}"
  js_set_label :: JSString -> Double -> IO ()



buildPlanetarium
  :: Event (Maybe CLineSet) -- ^ Load constellation lines
  -> Event ()               -- ^ Load HD catalog
  -> Behavior (Maybe Planetarium)
buildPlanetarium evtCL evtHD
  = make <$> (pure Nothing `switch` (pure        <$> evtCL))
         <*> (pure Nothing `switch` (pure . Just <$> evtHD))
  where
    make mCL mHD = do
      cl <- mCL
      mHD
      return $ Planetarium
        { clines      = cl
        , coordGrid   = simpleCoordGrid
        , brightStars = [ (fromSperical α δ, m)
                        | i <- [1 .. 272150]
                        , let α = catalogHDra   i
                        , let δ = catalogHDdec  i
                        , let m = catalogHDvisM i
                        , m < 5
                        ]
        }


----------------------------------------------------------------
-- Go!


adjustStream
  :: Num aa
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
  -- Load data
  evtClines <- loadCLines
  evtHD     <- loadCatalogHD
  -- Build planetarium 
  let bhvPlanetarium = buildPlanetarium evtClines evtHD
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
  -- bhvAAA <- pure $ pure (1/0)
  --           `switch`
  --                 (pure . fromIntegral . V.length <$> catHD)
  actimate (js_set_label "#lab-delta") $ bhvUD
  actimate (js_set_label "#lab-alpha") $ bhvLR
  actimate (js_set_label "#lab-zoom")  $ bhvZoom
  -- actimate (js_set_label "#lab-extra") $ bhvAAA

  ----------------------------------------------------------------
  bhvSize <- innerSizeBehavior "#area"
  flip actimate bhvSize $ \(w,h) ->
    runCanvas "cnv" $ resize w h
  -- Camera
  let makeCamera a d =
        let α = Angle a :: Angle Degrees Double
            δ = Angle d :: Angle Degrees Double
        in makeCameraRotation α δ
  let bhvCamera = Camera
               <$> (makeCamera <$> bhvLR <*> bhvUD)
               <*> bhvZoom
               <*> bhvSize
  -- Draw
  eLoaded <- sample $ whenJust bhvPlanetarium
  _ <- do cam <- sample bhvCamera
          let fun p = sync $ drawSky (Just p) cam
          planNow $ fun <$> eLoaded
  actimateB drawSky bhvPlanetarium bhvCamera 
  return ()
