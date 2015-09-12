-- |
-- Utils for working with camera viewport
module Planetarium.Camera where

import Data.Angle
import Data.Quaternion
import Celestial.Coordinates
import Celestial.Projection


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Which coordinate system is used for rendering
data CoordSystem
  = CoordEquatorial
  | CoordHorizontal

data Local

-- | Description of current camera
data Camera = Camera
  { cameraViewEq   :: CoordTransform Double (EquatorialCoord J1900) Local
    -- ^ Coordinate transform from equatorial to projection coordinates
  , cameraViewHor  :: CoordTransform Double  HorizonalCoord         Local
    -- ^ Coordinate transform from horizontal to projection coordinates
  , cameraZoom     :: Double
    -- ^ Zoom for camera
  , cameraViewport :: (Int,Int)
    -- ^ Size of camera viewport
  }
  deriving (Show,Eq)


----------------------------------------------------------------
-- Assembling camera view
----------------------------------------------------------------

-- | Create camera rotation which is centered at particular point at
--   the sky
makeCameraRotation
  :: (AngularUnit ta, AngularUnit td)
  => Angle ta Double -> Angle td Double -> Quaternion Double
makeCameraRotation α δ
  = rotX (3*pi / 2)
  * rotZ (pi   / 2)
  * rotY (asRadians δ)
  * rotZ (asRadians α)
