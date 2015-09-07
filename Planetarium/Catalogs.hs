{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
-- Catalogs support
module Planetarium.Catalogs (
    loadCatalogHD
  , catalogHDra
  , catalogHDdec
  ) where


import Data.Angle
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal

import Control.FRPNow

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
