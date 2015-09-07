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


----------------------------------------------------------------
-- JS

foreign import javascript safe "resize_canvas($1,$2)"
  js_resize_canvas :: Int -> Int -> IO ()

foreign import javascript safe "{$($1).empty(); $($1).append(''+$2);}"
  js_set_label :: JSString -> Double -> IO ()

foreign import javascript safe "load_catalogHD($1)"
  js_load_catalogHD :: JSFun (IO ()) -> IO ()

foreign import javascript safe "catalogHD.ra[$1]"
  catalogHDra  :: Int -> Angle HourRA Double
foreign import javascript safe "catalogHD.dec[$1]"
  catalogHDdec :: Int -> Angle Degrees Double


----------------------------------------------------------------
-- Reading catalogs
----------------------------------------------------------------

-- | Constellation line
newtype CLine = CLine { getCLine :: V.Vector (V.Vector Int) }
                deriving (FromJSON,Show)

type CLines = HM.HashMap T.Text CLine
  
data Planetarium = Planetarium
  { clines :: CLines
  }
  

buildPlanetarium
  :: Event (Maybe CLines) -- ^ Load constellation lines
  -> Event ()             -- ^ Load HD catalog
  -> Behavior (Maybe Planetarium)
buildPlanetarium evtCL evtHD
  = make <$> (pure Nothing `switch` (pure        <$> evtCL))
         <*> (pure Nothing `switch` (pure . Just <$> evtHD))
  where
    make mCL mHD = do
      cl <- mCL
      mHD
      return $ Planetarium { clines = cl }

data Camera = Camera
  { cameraView     :: Quaternion Double
  , cameraZoom     :: Double
  , cameraViewport :: (Int,Int)
  }
  deriving (Show,Eq)

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
  -- Draw grid
  duration "grid" $ forM_ ([-75, -65 .. 85 ] ++ [90]) $ \δ ->
    forM_ ([0, 10 .. 270] ++ [1 .. 9]) $ \α -> do
      let a,d :: Angle Degrees Double
          a = Angle α
          d = Angle δ
      case proj $ fromSperical a d of
        Just (ProjCoord (scale -> (x,y))) -> do
          fillRect x y 1 1
        Nothing -> return ()
  -- Draw constellation lines
  lineWidth 1
  duration "cline" $ forM_ (clines pl) $ \(CLine cl) -> do
    let mkPoint i = fromSperical (catalogHDra i) (catalogHDdec i)
    forM_ cl $ \contour ->
      case sequence $ proj . mkPoint <$> contour of
        Nothing -> return ()
        Just (fmap scale -> l) -> drawLine l



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
  evtClines :: Event (Maybe CLines) <- fetchJSON "data/clines.json"
  (evtHD,cbHD) <- callback
  jsCall <- sync $ syncCallback NeverRetain True $ cbHD ()
  sync $ js_load_catalogHD jsCall
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
  flip actimate bhvSize $ \(w,h) -> do
    js_resize_canvas w h
  -- Camera
  let makeCamera a d =
        let α = Angle a :: Angle Degrees Double
            δ = Angle d :: Angle Degrees Double
        in 1
           * rotX (3*pi/2)
           * rotZ (pi/2)
           * rotY (asRadians δ)
           * rotZ (asRadians α) :: Quaternion Double
  let bhvCamera = Camera
               <$> (makeCamera <$> bhvLR <*> bhvUD)
               <*> bhvZoom
               <*> bhvSize
  -- Draw
  actimateB drawSky bhvPlanetarium bhvCamera 
  return ()
