{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE JavaScriptFFI              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Category    ((<<<))
import Control.Applicative
import Control.Monad hiding (forM_,sequence)
import Control.FRPNow
import Control.Concurrent
import Control.Exception
import Data.Angle
import Data.Monoid
import Data.Traversable (sequence)
import Data.Time     (parseTime,formatTime)
import System.Locale (defaultTimeLocale)
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign

import Celestial.Coordinates
import Celestial.Geo
import Celestial.Time

import JavaScript.FRP
import JavaScript.Canvas
import JavaScript.Utils

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
  = make <$>
    -- (pure (Just mempty))
    (pure Nothing `switch` (pure        <$> evtCL))
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
                        , Just α <- [catalogHDra   i]
                        , Just δ <- [catalogHDdec  i]
                        , Just m <- [catalogHDvisM i]
                        , m < 6
                        ]
        }


----------------------------------------------------------------
-- Go!

-- | Start renderer thread which takes values from MSink
startRendererThread :: IO (Planetarium -> Camera -> IO ())
startRendererThread = do
  drawSink <- newMSink
  _ <- forkIO $ forever $ do
    e <- try $ do
      (pln,cam) <- takeMSink drawSink
      duration "sky" $ runCanvas "cnv" $ drawSky (Just pln) cam
    case e of
      Left err -> consoleLog $ show (err :: SomeException)
      _        -> return ()
  return $ \pln cam -> putMSink drawSink (pln,cam)



-- | Coordinate system
data Coords
  = CEquatorial
  | CHorizontal
  deriving (Show,Eq)

-- | Command for changing coordinates
data PointingCmd
  = MoveLR Double
    -- ^ Move in left-right direction
  | MoveUD Double
    -- ^ Move in up-down direction
  | ChangeTo Location JD Coords
    -- ^

-- | Current pointing
data Pointing
  = PEq  Double Double
  | PHor Double Double
  deriving (Eq,Show)


hor2eq :: Location -> JD -> (Double,Double) -> (Double,Double)
hor2eq loc jd (a,h)
  = (getAngle α, getAngle δ)
  where
  aa,hh,α,δ :: Angle Degrees Double
  aa = angle a
  hh = angle h
  --
  p :: Spherical HorizonalCoord Double
  p = fromSpherical aa hh
  --
  lst = meanLST loc jd
  p' = toCoord (horizontalToEquatorial loc lst) p
  (α,δ) = toSpherical p'

eq2hor :: Location -> JD -> (Double,Double) -> (Double,Double)
eq2hor loc jd (α,δ)
  = (getAngle aa, getAngle hh)
  where
  aa,hh,αα,δδ :: Angle Degrees Double
  αα = angle α
  δδ = angle δ
  --
  p :: Spherical (EquatorialCoord B1900) Double
  p = fromSpherical αα δδ
  --
  lst = meanLST loc jd
  p' = toCoord (equatorialToHorizontal loc lst) p
  (aa,hh) = toSpherical p'


main :: IO ()
main = runNowMaster' $ do
  -- Start rendered thread
  drawCmd <- sync startRendererThread

  ----------------------------------------------------------------
  -- Planetarium data
  --
  -- Load data
  evtClines <- loadCLines
  evtHD     <- loadCatalogHD
  -- Build planetarium
  let bhvPlanetarium = buildPlanetarium evtClines evtHD

  ----------------------------------------------------------------
  -- Controls
  ----------------------------------------------------------------

  -- Zoom
  bhvZoom <- do
    streamZ1 <- adjustStream ("#btn-zoomout", (/1.1)) ("#btn-zoomin",(*1.1))
    streamZ2 <- let trans d | d < 0     = (*1.02)
                            | otherwise = (/1.02)
                in fmap trans <$> wheelEventStream "#cnv"
    sample $ foldEs (\n f -> f n) 1 (streamZ1 <> streamZ2)
  -- Viewport size
  bhvSize <- innerSizeBehavior "#area"
  -- Location
  bhvLoc <- do
    (_,bhvLat)  <- numberStream "#inp-lat"  55 ( -90,  90)
    (_,bhvLong) <- numberStream "#inp-long" 37 (-180, 180)
    return $ Location <$> (angle <$> bhvLat) <*> (angle <$> bhvLong)
  -- Time  
  bhvTime <- do
    let fmt = "%Y/%m/%d %H:%M"
    jd <- sync currentJD
    --
    sync $ jquerySetVal "#inp-datetime" $ formatTime defaultTimeLocale fmt $ jd2UTC jd
    evts <- streamChange "#inp-datetime"
    sample $ fromChanges jd
           $ fmap utc2JD
           $ catMaybesEs
           $ parseTime defaultTimeLocale fmt . fromJSString <$> evts
  -- Projection
  bhvProj <- do
    evts <- streamSelectInput [ ("Orthographic" , ProjOrthographic)
                              , ("Stereographic", ProjSterographic)
                              , ("Gnomonic"     , ProjGnomonic    )
                              , ("Az. equidistant", ProjAzimuthalEquidistant)
                              ] "#inp-proj"
    sample $ fromChanges ProjOrthographic evts
  -- Location

  ----------------------------------------
  -- Pointing
  bhvPoint <- do
    -- Coordinate system selection
    evtsCoord <- ((ChangeTo <$> bhvLoc <*> bhvTime) <@@>)
              <$> streamSelectInput
      [ ("Eq. coord.",  CEquatorial)
      , ("Hor. coord.", CHorizontal)
      ] "#inp-coord"
    -- Movement
    evtsMove <- mconcat <$> sequence
      [ ((MoveUD (-10)) <$) <$> onClickStream "#btn-down"
      , ((MoveUD   10 ) <$) <$> onClickStream "#btn-up"
      , ((MoveLR (-10)) <$) <$> onClickStream "#btn-left"
      , ((MoveLR   10 ) <$) <$> onClickStream "#btn-right"
      ]
    let -- Movements
        step (PEq  a d) (MoveLR dx) = PEq  (a+dx) d
        step (PEq  a d) (MoveUD dx) = PEq  a (d+dx)
        step (PHor a d) (MoveLR dx) = PHor (a+dx) d
        step (PHor a d) (MoveUD dx) = PHor a (d+dx)
        -- Coordinate system change
        step x@PEq{}  (ChangeTo _ _ CEquatorial) = x
        step x@PHor{} (ChangeTo _ _ CHorizontal) = x
        step (PEq  a d) (ChangeTo loc jd CHorizontal) =
          uncurry PHor $ eq2hor loc jd (a,d)
        step (PHor a d) (ChangeTo loc jd CEquatorial) =
          uncurry PEq $ hor2eq loc jd (a,d)
    sample $ foldEs step (PEq 0 0)
           $ evtsCoord <> evtsMove

  ----------------------------------------------------------------
  -- Camera
  let bhvCamera = do
        p    <- bhvPoint
        loc  <- bhvLoc
        jd   <- bhvTime
        zoom <- bhvZoom
        view <- bhvSize
        prj  <- bhvProj
        -- Look camera
        let lst = meanLST loc jd
        let (camEq,camHor) = case p of
              PEq  a d ->
                let cam = lookAt
                            (angle a :: Angle Degrees Double)
                            (angle d :: Angle Degrees Double)
                in ( cam
                   , cam <<< horizontalToEquatorial loc lst)
              PHor a d ->
                let cam = lookAt
                            (angle a :: Angle Degrees Double)
                            (angle d :: Angle Degrees Double)
                in ( cam <<< equatorialToHorizontal loc lst
                   , cam )
        return $ Camera
          { cameraViewEq     = camEq
          , cameraViewHor    = camHor
          , cameraZoom       = zoom
          , cameraViewport   = view
          , cameraProjection = prj
          }

  -- Report status of camera
  -- actimate (js_set_label "#lab-delta") $ bhvUD
  -- actimate (js_set_label "#lab-alpha") $ bhvLR
  actimate (consoleLog . show)  $ bhvZoom
  -- Resize canvas when needed
  flip actimate bhvSize $ \(w,h) ->
    runCanvas "cnv" $ resize w h
  -- Draw sky
  eLoaded <- sample $ whenJust bhvPlanetarium
  _ <- do cam <- sample bhvCamera
          planNow $ sync . (\p -> drawCmd p cam) <$> eLoaded
  actimateB (\ma b -> case ma of
               Just a  -> do consoleLog "DRAW!"
                             drawCmd a b
               Nothing -> return ()
            )
    bhvPlanetarium bhvCamera
  return ()
