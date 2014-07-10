module Main where

import Graphics.Gloss
import Debug.Trace

windowSize = 400.0

main
 = do display window white draw

window = InWindow "Hello World" (floor windowSize, floor windowSize) (0, 0)
type V3 = (Float, Float, Float)
type V2 = (Float, Float)

type WorldPoint  = V3
type WorldVector = V3

type CameraPoint = V2
type ScreenPoint = V2

data Shape
  = Sphere { center :: WorldPoint, radius :: Float }
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
zw2 f (a1, a2) (b1, b2)         = (f a1 b1, f a2 b2)
zw3 f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

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
      in [(x, y) | x <- axis, y <- axis]

magnitude :: WorldVector -> Float
magnitude v = sqrt $ dot v v

dot :: V3 -> V3 -> Float
dot (x,y,z) (a,b,c) = x*a + y*b + z*c

add :: V3 -> V3 -> V3
add v1 v2 = zw3 (+) v1 v2

sub :: V3 -> V3 -> V3
sub v1 v2 = zw3 (-) v1 v2

mult :: V3 -> Float -> V3
mult v1 m = m3 (*m) v1

distance :: WorldPoint -> WorldPoint -> Float
distance p1 p2 = magnitude $ sub p1 p2

normalize :: V3 -> V3
normalize v = m3 (/ (magnitude v)) v

intersects :: Shape -> WorldPoint -> WorldVector -> Maybe WorldPoint
intersects sphere p v =
  let r = radius sphere
      o = center sphere
      l = sub o p
      tca = dot l (normalize v)
      d2 = (dot l l) - (tca ^ 2)
      in if (tca < 0) || (d2 > r^2)
         then Nothing
         else let thc = sqrt(r^2 - d2)
                  t = tca - thc
              in Just $ add p $ mult v t

surfaceNormal :: Shape -> WorldPoint -> WorldVector
surfaceNormal sphere p = normalize $ sub p $ center sphere

-- functions for testing drawing
cycledColors :: [Color]
cycledColors = cycle [red, yellow, blue, green, black, orange]

drawBoxes :: Float -> Float -> Float -> [Picture]
drawBoxes x y r = do
  let zs = [20.0, 19.0.. 5.0]
      rect = [(x+r,y+r), (x+r,y-r), (x-r,y-r), (x-r,y+r)]
      rects = map (\z -> map (\(x,y) -> (x,y,z)) rect) zs
      screenRects = map (\rect -> map worldToScreen rect) rects
      in zipWith (\rect color -> Color color $ Polygon rect)
                 screenRects cycledColors

drawLotsOfBoxes =
  Pictures ((drawBoxes (-2) (-2) 1) ++
            (drawBoxes 5 1 1) ++
            (drawBoxes (-3) 4 1) ++
            (drawBoxes (4) (-4) 1))

drawColoredPixels
  = Pictures
  $ zipWith (\c p -> coloredPixel c p) cycledColors screenPoints

drawReflection :: Shape -> Maybe WorldPoint -> Color
drawReflection sphere intersection =
  case intersection of
    Nothing -> black
    Just point -> let
      lightdir = (5,5,-5)
      normal = surfaceNormal sphere point
      in greyN $ dot (normalize normal) (normalize lightdir)

drawRay :: WorldPoint -> WorldVector -> Picture
drawRay origin ray =
  let sphere = Sphere {center = (0,0,10), radius = 5}
      intersection = intersects sphere origin ray
      color = drawReflection sphere intersection
      screenRay = worldToScreen ray
      in coloredPixel color screenRay

draw :: Picture
draw =
  let rays = map cameraToWorld $ map screenToCamera screenPoints
      in Pictures $ map (drawRay (0, 0, 0)) rays
