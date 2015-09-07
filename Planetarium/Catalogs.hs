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
  ) where

import Control.Applicative
import Control.FRPNow

import Data.Angle
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Data.HashMap.Strict as HM

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import Celestial.Coordinates

import Web.JQ



----------------------------------------------------------------
-- Constellation lines
----------------------------------------------------------------

-- | Constellations lines for one constellation.
newtype CLines = CLines
  { getCLines :: V.Vector (V.Vector (Spherical (EquatorialCoord J1900) Double)) }

type CLineSet = HM.HashMap T.Text CLines

-- | Load constellation lines
loadCLines :: Now (Event (Maybe CLineSet))
loadCLines
  = (fmap . fmap . fmap) (CLines . (V.map . V.map) make)
 <$> fetchJSON "data/clines.json"
  where
    make i = fromSperical (catalogHDra i) (catalogHDdec i)
               

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

foreign import javascript safe "catalogHD.ra[$1]"
  catalogHDra  :: Int -> Angle HourRA Double
foreign import javascript safe "catalogHD.dec[$1]"
  catalogHDdec :: Int -> Angle Degrees Double
