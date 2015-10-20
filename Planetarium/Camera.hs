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

data ProjType
  = ProjOrthographic
  | ProjSterographic
  | ProjGnomonic
  | ProjAzimuthalEquidistant
  | ProjAzimuthalEqArea
  deriving (Show,Eq)

toProjection :: ProjType -> Projection Double
toProjection prj = case prj of
  ProjOrthographic         -> orthographic
  ProjSterographic         -> stereographic
  ProjGnomonic             -> gnomonic
  ProjAzimuthalEquidistant -> azimuthalEquidistant
  ProjAzimuthalEqArea      -> azimuthalEqualArea


-- | Description of current camera
data Camera = Camera
  { cameraViewEq   :: CoordTransform Double (EquatorialCoord B1900) Proj
    -- ^ Coordinate transform from equatorial to projection coordinates
  , cameraViewHor  :: CoordTransform Double  HorizonalCoord         Proj
    -- ^ Coordinate transform from horizontal to projection coordinates
  , cameraZoom     :: Double
    -- ^ Zoom for camera
  , cameraViewport :: (Int,Int)
    -- ^ Size of camera viewport
  , cameraProjection :: ProjType
  }
  deriving (Show,Eq)
