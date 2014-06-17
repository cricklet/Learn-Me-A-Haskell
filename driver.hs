module Main where

import Graphics.Gloss
import Debug.Trace

windowSize = 400.0

main
 = do display window white draw

window = InWindow "Hello World" (floor windowSize, floor windowSize) (0, 0)

type WorldPoint  = (Float, Float, Float)
type CameraPoint = (Float, Float)
type ScreenPoint = (Float, Float)
data Shape
  = Sphere { center :: WorldPoint, radius :: Float }
  | Cuboid { minPoint :: WorldPoint, maxPoint :: Float }
  deriving (Show)

-- convert between coordinate systems
worldToCamera (x, y, z) = (x / z, y / z)
cameraToScreen point
  = m2 (/ 2.0)
  $ m2 (* windowSize) point
worldToScreen = cameraToScreen . worldToCamera

cameraToWorld (x, y) = (x, y, 1)
screenToCamera point
  = m2 (* 2.0)
  $ m2 (/ windowSize) point

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
  = rect (m2 (+ (-0.5)) point) (m2 (+0.5) point)

coloredPixel :: Color -> ScreenPoint -> Picture
coloredPixel color p
  = (Color color) (pixel p)

-- ray tracing code
screenPoints :: [ScreenPoint]
screenPoints = do
  let axis = map ((-) (windowSize / 2)) [0..windowSize]
  [(x, y) | x <- axis, y <- axis]

-- functions for testing drawing
cycledColors :: [Color]
cycledColors = cycle [red, yellow, blue, green, black, orange]

drawBoxes :: Float -> Float -> Float -> [Picture]
drawBoxes x y r = do
  let zs = [20.0, 19.0.. 1.0]
  let rect = [(x+r,y+r), (x+r,y-r), (x-r,y-r), (x-r,y+r)]
  let rects = map (\z -> map (\(x,y) -> (x,y,z)) rect) zs
  let screenRects = map (\rect -> map worldToScreen rect) rects

  zipWith (\rect color -> Color color $ Polygon rect)
          screenRects cycledColors

drawLotsOfBoxes =
  Pictures ((drawBoxes (-2) (-2) 1) ++
            (drawBoxes 5 1 1) ++
            (drawBoxes (-3) 4 1) ++
            (drawBoxes (4) (-4) 1))

drawColoredPixels
  = Pictures
  $ zipWith (\c p -> coloredPixel c p) cycledColors screenPoints

drawSphere
  = Pictures
  $ zipWith (\c p -> coloredPixel c p) cycledColors
  $ filter (\(x, y) -> x < 0 && y < 0) screenPoints

draw :: Picture
draw
  = drawSphere

