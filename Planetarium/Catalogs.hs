{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Catalogs support
module Planetarium.Catalogs (
    -- * Constellation lines
    CLines(..)
  , CLineSet
  , loadCLines
    -- * HD catalog
  , loadCatalogHD
  , catalogHDra
  , catalogHDdec
  , catalogHDvisM
  ) where

import Control.Applicative
import Control.FRPNow

import Data.Angle
import Data.Maybe
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import System.IO.Unsafe (unsafePerformIO)

import Celestial.Coordinates

import JavaScript.Utils



----------------------------------------------------------------
-- Constellation lines
----------------------------------------------------------------

-- | Constellations lines for one constellation.
newtype CLines = CLines
  { getCLines :: V.Vector (V.Vector (Spherical (EquatorialCoord B1900) Double)) }

type CLineSet = HM.HashMap T.Text CLines

-- | Load constellation lines
loadCLines :: Now (Event (Maybe CLineSet))
loadCLines
  -- = undefined
  = (fmap . fmap . fmap) (CLines . (V.map . V.map) make)
 <$> fetchJSON "data/constellation-lines-coord.json"
  where
    make (a,d) = fromSpherical (angle a :: Angle HourRA  Double)
                               (angle d :: Angle Degrees Double)
 --    makeLine v = V.mapM make v
                 
 --    make i = do α <- catalogHDra  i
 --                δ <- catalogHDdec i
 --                fromSpherical α δ

----------------------------------------------------------------
-- Catalog access
----------------------------------------------------------------

-- | Load HD catalog and generate event when it's loaded
loadCatalogHD :: Now (Event ())
loadCatalogHD = do
  (evtHD,cbHD) <- callback
  sync . js_load_catalogHD =<< sync (syncCallback NeverRetain True $ cbHD ())
  return evtHD

----------------------------------------------------------------
-- FFI
----------------------------------------------------------------

foreign import javascript safe "load_catalogHD($1)"
  js_load_catalogHD :: JSFun (IO ()) -> IO ()

catalogHDra :: Int -> Maybe (Angle HourRA Double)
catalogHDra n = unsafePerformIO $ do
  ma <- fromJSRef $ js_catalogHDra n
  return $ fmap angle ma

catalogHDdec :: Int -> Maybe (Angle Degrees Double)
catalogHDdec n = unsafePerformIO $ do
  ma <- fromJSRef $ js_catalogHDdec n
  return $ fmap angle ma

catalogHDvisM :: Int -> Maybe Double
catalogHDvisM = unsafePerformIO . fromJSRef . js_catalogHDvisM

foreign import javascript safe "catalogHD.ra[$1]"
  js_catalogHDra  :: Int -> JSRef Double
foreign import javascript safe "catalogHD.dec[$1]"
  js_catalogHDdec :: Int -> JSRef Double
foreign import javascript safe "catalogHD.m[$1]"
  js_catalogHDvisM :: Int -> JSRef Double

