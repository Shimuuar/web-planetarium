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


----------------------------------------------------------------
-- JS
----------------------------------------------------------------

foreign import javascript safe "{$($1).empty(); $($1).append(''+$2);}"
  js_set_label :: JSString -> Double -> IO ()



----------------------------------------------------------------
-- Reading catalogs
----------------------------------------------------------------

data Planetarium = Planetarium
  { clines    :: CLineSet
  , coordGrid :: [[Spherical (EquatorialCoord J1900) Double]]
  }


simpleCoordGrid :: [[Spherical (EquatorialCoord J1900) Double]]
simpleCoordGrid =
  [ [ fromSperical (Angle α :: Angle Degrees Double) (Angle δ :: Angle Degrees Double)
    | α <- [0,10 .. 360]
    ]
  | δ <- [-80,-70 .. 80]
  ] ++
  [ [ fromSperical (Angle α :: Angle Degrees Double) (Angle δ :: Angle Degrees Double)
    | δ <- [-90,-70 .. 90]
    ]
  | α <- [0,10 .. 360]
  ]

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
      return $ Planetarium { clines    = cl
                           , coordGrid = simpleCoordGrid
                           }

drawSky :: Maybe Planetarium -> Camera -> IO ()
drawSky Nothing _ = return ()
drawSky (Just pl) (Camera cam zoom (w,h)) = duration "sky" $ runCanvas "cnv" $ do
  clear
  let proj (Spherical v) = (project orthographic . Spherical . rotateVector cam) v
      scale p = let (x,y) = F.convert p
                    ss = zoom * fromIntegral (min w h) / 2.2
                    xx = ss * x + fromIntegral w / 2
                    yy = fromIntegral h / 2 - ss * y
                in (xx,yy)
  fillStyle "#aaf"
  arc (fromIntegral w/2, fromIntegral h/2) (zoom * fromIntegral (min w h) / 2.2 * 1) (0,2*pi)
  fill
  -- Draw grid
  beginPath
  lineWidth 0.5
  strokeStyle "#ccf"
  duration "grid" $ forM_ (coordGrid pl) $ \ln -> do
    drawprojLine (fmap scale . proj) ln
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
  actimateB drawSky bhvPlanetarium bhvCamera 
  return ()
