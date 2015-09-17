{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Category    ((<<<))
import Control.Applicative
import Control.Monad hiding (forM_,sequence)
import Control.Monad.IO.Class
import Control.FRPNow
import Control.Concurrent
import Control.Exception
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
import Celestial.Geo
import Celestial.Time

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
        { clines       = cl
        , coordGridEq  = simpleCoordGrid
        , coordGridHor = simpleCoordGrid
        , brightStars = [ (fromSpherical α δ, m)
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


data MSink a = MSink (MVar a) (MVar ())

newMSink :: IO (MSink a)
newMSink = MSink <$> newEmptyMVar <*> newMVar ()

putSink :: MSink a -> a -> IO ()
putSink (MSink mv sem) a = do
  -- () <- takeMVar sem
  _  <- tryTakeMVar mv
  putMVar mv  a
  -- putMVar sem ()

getSink :: MSink a -> IO a
getSink (MSink mv _) = do
  takeMVar mv



main :: IO ()
main = runNowMaster' $ do
  drawSink <- sync newMSink
  -- Start rendered thread
  _ <- sync $ forkIO $ forever $ do
    e <- try $ do
      consoleLog "Prepare!"
      (pln,cam) <- getSink drawSink
      duration "sky" $ runCanvas "cnv" $ drawSky (Just pln) cam
    consoleLog $ show (e :: Either SomeException ())
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
  streamZ1 <- adjustStream ("#btn-zoomout", (/1.1)) ("#btn-zoomin",(*1.1))
  streamZ2 <- let trans d | d < 0     = (*1.02)
                          | otherwise = (/1.02)
              in fmap trans <$> wheelEventStream "#cnv"
  bhvZoom <- sample $ foldEs (\n f -> f n) 1 (streamZ1 <> streamZ2)
  --
  actimate (js_set_label "#lab-delta") $ bhvUD
  actimate (js_set_label "#lab-alpha") $ bhvLR
  actimate (js_set_label "#lab-zoom")  $ bhvZoom

  ----------------------------------------------------------------
  bhvSize <- innerSizeBehavior "#area"
  flip actimate bhvSize $ \(w,h) ->
    runCanvas "cnv" $ resize w h
  -- Camera
  -- let makeCamera a d =
  --       let α = Angle a :: Angle Degrees Double
  --           δ = Angle d :: Angle Degrees Double
  --       in makeCameraRotation α δ
  let locMoscow = Location (angle 55) (angle 37)
  jd <- sync currentJD
  --
  let lst = meanLST locMoscow jd
  let bhvCamera = do
        a    <- bhvLR
        d    <- bhvUD
        let cam = lookAtEquatorial (angle a :: Angle Degrees Double)
                                   (angle d :: Angle Degrees Double)
        zoom <- bhvZoom
        view <- bhvSize
        return $ Camera
          { cameraViewEq   = cam
          , cameraViewHor  = cam <<< horizontalToEquatorial locMoscow lst
          , cameraZoom     = zoom
          , cameraViewport = view
          }
  -- Draw
  eLoaded <- sample $ whenJust bhvPlanetarium
  _ <- do cam <- sample bhvCamera
          planNow $ sync . (\p -> putSink drawSink (p,cam)) <$> eLoaded
  actimateB (\ma b -> case ma of
               Just a  -> do consoleLog "DRAW!"
                             putSink drawSink (a,b)
               Nothing -> return ()
            )
    bhvPlanetarium bhvCamera
  return ()
