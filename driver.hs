import Graphics.Gloss
import Debug.Trace

windowSize = 400
renderScale = 4

main
 = do display window white draw

window = InWindow "Hello World" (windowSize, windowSize) (0, 0)

type Point3D = (Float, Float, Float)
type Point2D = (Float, Float)
type Vector3D = (Float, Float, Float)
data Shape = Sphere { center :: Point3D, radius :: Float }
           | Cuboid { minPoint :: Point3D, maxPoint :: Float }
            deriving (Show)

zipTuple2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
zipTuple2 f (a1, b1) (a2, b2)
  = (f a1 a2, f b1 b2)

zipTuple3 :: (a -> a -> a) -> (a, a, a) -> (a, a, a) -> (a, a, a)
zipTuple3 f (a1, b1, c1) (a2, b2, c2)
  = (f a1 a2, f b1 b2, f c1 c2)

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

intersect :: Point3D -> Vector3D -> Shape -> Maybe Color
intersect start dir Sphere { center=center, radius=radius }
  = Nothing
intersect start dir Cuboid { minPoint=minPoint, maxPoint=maxPoint }
  = Nothing

coloredLine :: Picture
coloredLine
  = Pictures
  $ zipWith (\c p -> coloredPixel c p)
      [red, yellow, blue, green, black, orange]
      (zip [1..5] [1..5])

screenPoints :: Point2D -> Point2D -> [Point2D]
screenPoints (xMin, yMin) (xMax, yMax)
  = [ (x, y) | x <- [xMin..xMax], y <- [yMin..yMax] ]

cycledColors :: [Color]
cycledColors = cycle [red, yellow, blue, green, black, orange]

coloredRect :: Point2D -> Point2D -> Picture
coloredRect minPoint maxPoint
  = Pictures
  $ zipWith (\c p -> coloredPixel c p)
            cycledColors (screenPoints minPoint maxPoint)

drawShape :: Shape -> Picture
drawShape shape
  = coloredPixel white (0, 0)

-- Use a resolution of 20 x 20 with 0,0 in the center

draw :: Picture
draw = do let shape = Sphere { center = (0, 0, 10), radius = 5 }
          Pictures $ [coloredRect (-50, -50) (50, 50)] ++ [drawShape shape]
