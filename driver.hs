import Graphics.Gloss

windowSize = 200
pixelsSize = 20

renderScale = windowSize / pixelsSize

main
 = display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

drawRect :: Float -> Float -> Float -> Float -> Picture
drawRect x0 y0 x1 y1
  = Polygon [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]

drawPixel :: Float -> Float -> Picture
drawPixel x y
  = drawRect x y (x+1) (y+1)

testPixel :: Picture
testPixel
  = Translate 0 0
  $ Scale renderScale renderScale
  $ Color black
  $ drawRect 0 0 1 1

testPixels :: Picture
testPixels
  = Pictures $ map (drawPixel 0) [0..20]

-- Use a resolution of 20 x 20 with 0,0 in the center

draw :: Picture
draw = testPixels

