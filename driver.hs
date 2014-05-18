import Graphics.Gloss
import Debug.Trace

windowSize = 400
renderScale = 4

main
 = do display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

type Point3D = (Float, Float, Float)
type Point2D = (Float, Float)
data Shape = Sphere { center :: Point3D, radius :: Float }
           | Cuboid { minPoint :: Point3D, maxPoint :: Float }
            deriving (Show)

rect :: Point2D -> Point2D -> Picture
rect (x0, y0) (x1, y1)
  = Polygon [(x0, y0), (x0, y1), (x1, y1), (x1, y0)]

pixel :: Point2D -> Picture
pixel (x, y)
  = Scale renderScale renderScale
  $ rect (x - 0.5, y - 0.5) (x + 0.5, y + 0.5)

coloredPixel :: Color -> Point2D -> Picture
coloredPixel color p
  = (Color color) (pixel p)

intersect :: Shape -> Maybe Color
intersect Sphere { center=center, radius=radius } = Nothing
intersect Cuboid { minPoint=minPoint, maxPoint=maxPoint } = Nothing

coloredLine :: Picture
coloredLine
  = Pictures
  $ zipWith (\c p -> coloredPixel c p)
      [red, yellow, blue, green, black, orange]
      (zip [1..5] [1..5])

coloredRect :: Point2D -> Point2D -> Picture
coloredRect (xMin, yMin) (xMax, yMax)
  = Pictures
  $ zipWith (\c p -> coloredPixel c p) colors points
  where colors = cycle [red, yellow, blue, green, black, orange]
        points = [ (x, y) | x <- [xMin..xMax], y <- [yMin..yMax] ]

-- Use a resolution of 20 x 20 with 0,0 in the center

draw :: Picture
draw = do let shape = Sphere { center = (0, 0, 10), radius = 5 }
          coloredRect (-50, -50) (50, 50)
