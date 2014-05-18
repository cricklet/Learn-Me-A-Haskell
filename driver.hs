import Graphics.Gloss

windowSize = 400
renderScale = 5

main
 = do let shape = testShape
      display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

type Point3D = (Float, Float, Float)
data Shape = Sphere { center :: Point3D, radius :: Float } deriving (Show)

rect :: Float -> Float -> Float -> Float -> Picture
rect x0 y0 x1 y1
  = Polygon [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]

pixel :: Float -> Float -> Picture
pixel x y
  = Scale renderScale renderScale
  $ rect (x - 0.5) (y - 0.5) (x + 0.5) (y + 0.5)

coloredPixel color x y
  = (Color color) (pixel x y)

testPixel :: Picture
testPixel
  = Color black
  $ pixel 0 0

testPixels :: Picture
testPixels
  = Pictures
  $ zipWith3 (\c x y -> coloredPixel c x y)
      [red, yellow, blue, green, black, orange]
      [1..5] [1..5]

testShape
  = Sphere { center = (0, 0, 10), radius = 5 }

-- Use a resolution of 20 x 20 with 0,0 in the center

draw :: Picture
draw = testPixels

