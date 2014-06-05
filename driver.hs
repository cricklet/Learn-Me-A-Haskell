module Main where

import Graphics.Gloss
import Debug.Trace

windowSize = 400

main
 = do display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

type WorldPoint  = (Float, Float, Float)
type WorldVector = (Float, Float, Float)
type CameraPoint = (Float, Float)
type ScreenPoint = (Float, Float)
data Shape
  = Sphere { center :: WorldPoint, radius :: Float }
  | Cuboid { minPoint :: WorldPoint, maxPoint :: Float }
  deriving (Show)

-- convert between coordinate systems
worldToCamera (x, y, z) = (x / z, y / z)
cameraToScreen point
  = m2 (/ fromIntegral 2)
  $ m2 (* fromIntegral windowSize) point
worldToScreen point = cameraToScreen . worldToCamera

-- zip functions
z2 f (a1, a2) (b1, b2)         = (f a1 b1, f a2 b2)
z3 f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

-- map functions
m2 f (a1, a2)     = (f a1, f a2)
m3 f (a1, a2, a3) = (f a1, f a2, f a3)

-- draw on the screen
rect :: ScreenPoint -> ScreenPoint -> Picture
rect (x0, y0) (x1, y1)
  = Polygon [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]

pixel :: ScreenPoint -> Picture
pixel point
  = rect (m2 ((-)0.5) point) (m2 (+0.5) point)

coloredPixel :: Color -> ScreenPoint -> Picture
coloredPixel color p
  = (Color color) (pixel p)

-- functions for testing drawing
cycledColors :: [Color]
cycledColors = cycle [red, yellow, blue, green, black, orange]

draw :: Picture
draw = do let a = (0.5, 0.5) :: CameraPoint
          let b = (0.7, 0.7) :: CameraPoint
          let r1 = Color blue $ rect (cameraToScreen a) (cameraToScreen b)
          --let c = (-5, -5, 10) :: WorldPoint
          --let d = (-2, -2, 10) :: WorldPoint
          --let r2 = Color red $ rect (worldToScreen c) (worldToScreen d)
          --Pictures [r1, r2]
          r1
