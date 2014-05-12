import Graphics.Gloss

windowSize = 200
pixelsSize = 20

-- renderScale = windowSize / pixelsSize
renderScale = 10

main
 = display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

rect :: Float -> Float -> Float -> Float -> Picture
rect x0 y0 x1 y1
  = Polygon [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]

pixel :: Float -> Float -> Picture
pixel x y
  = Scale renderScale renderScale
  $ rect (x - 0.5) (y - 0.5) (x + 0.5) (y + 0.5)

coloredPixel x y
  = (Color red) (pixel x y)

testPixel :: Picture
testPixel
  = Color black
  $ pixel 0 0

testPixels :: Picture
testPixels
  = Pictures
  $ map (coloredPixel 0) [1..2]

-- Use a resolution of 20 x 20 with 0,0 in the center

draw :: Picture
draw = testPixels

