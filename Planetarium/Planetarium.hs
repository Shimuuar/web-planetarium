-- |
module Planetarium.Planetarium where

import Data.Angle
import Celestial.Coordinates

import Planetarium.Catalogs

----------------------------------------------------------------
-- Planetary data
----------------------------------------------------------------

-- | Immutable data for planetarium
data Planetarium = Planetarium
  { clines       :: CLineSet
    -- ^ List of constellation lines
  , coordGridEq  :: [[Spherical (EquatorialCoord J1900) Double]]
    -- ^ Coordinate grid
  , coordGridHor :: [[Spherical HorizonalCoord Double]]
    -- ^ Coordinate grid
  , brightStars  :: [(Spherical (EquatorialCoord J1900) Double,Double)]
    -- ^ List of bright stars (brighter than 5m)
  }

-- | Coordinate grid with fixed step
simpleCoordGrid :: [[Spherical c Double]]
simpleCoordGrid =
  [ [ cast $ fromSpherical (angle α :: Angle Degrees Double) (angle δ :: Angle Degrees Double)
    | α <- [0,10 .. 360]
    ]
  | δ <- [-80,-70 .. 80]
  ] ++
  [ [ cast $ fromSpherical (angle α :: Angle Degrees Double) (angle δ :: Angle Degrees Double)
    | δ <- [-88] ++ [-80,-70 .. 80] ++ [88]
    ]
  | α <- [0,10 .. 360]
  ]
  where
    cast :: Spherical HorizonalCoord a -> Spherical c a
    cast (Spherical a) = Spherical a
