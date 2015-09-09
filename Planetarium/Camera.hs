-- |
-- Utils for working with camera viewport
module Planetarium.Camera where

import Data.Angle
import Data.Quaternion

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Description of current camera
data Camera = Camera
  { cameraView     :: Quaternion Double
  , cameraZoom     :: Double
  , cameraViewport :: (Int,Int)
  }
  deriving (Show,Eq)

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
