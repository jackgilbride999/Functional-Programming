module Shapes(
  Shape, Point, Vector, Transform, Drawing, ColoredDrawing, Color,
  mask,
  r, g, b, a,
  point, getX, getY,
  color, red, green, blue, transparent,
  colorPixel,
  empty, circle, square, rectangle, ellipse, convexPolygon,
  identity, translate, rotate, scale, (<+>),
  inside, 
  next, mandelbrot, fairlyClose, inMandelbrotSet, approxTest, mandelbrotDrawing)  where

import Data.Word

-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

add :: Matrix -> Matrix -> Matrix
add (Matrix (Vector a1 b1) (Vector c1 d1)) (Matrix (Vector a2 b2) (Vector c2 d2)) = Matrix (Vector (a1+a2) (b1+b2)) (Vector (c1+c2) (d1+d2))

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector

type AspectRatio = Double
data Color = Color {
  r :: Word8, 
  g :: Word8, 
  b:: Word8, 
  a ::Word8
}

color r g b a = Color r g b a
red = Color 255 0 0 255
green = Color 0 255 0 255
blue = Color 0 0 255 255
transparent = Color 255 255 255 0

data Shape = Empty 
           | Circle 
           | Square
           | Rectangle AspectRatio
           | Ellipse AspectRatio
           | ConvexPolygon [Point]
             deriving Show

empty, circle, square :: Shape
rectangle, ellipse :: Double -> Double -> Shape
convexPolygon :: [Point] -> Shape

empty = Empty
circle = Circle
square = Square
rectangle width height = Rectangle (width/height)
ellipse width height = Ellipse (width/height)
convexPolygon (p:ps) = ConvexPolygon (p:ps)

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

Translate (Vector x1 y1) <+> Translate (Vector x2 y2) = Translate (Vector (x1+x2) (y1+y2))
Scale (Vector x1 y1) <+> Scale (Vector x2 y2) = Scale (Vector (x1+x2) (y1+y2))
Rotate angle1 <+> Rotate angle2 = Rotate (angle1 `add` angle2)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

type Drawing = [(Transform,Shape)]
type ColoredDrawing = [(Transform, Shape, Color)]

mask :: Word8 -> ColoredDrawing -> ColoredDrawing -> ColoredDrawing
mask _ _ [] = []
mask transparency [] ((mt, ms, mc) : masks) = (mt, ms, (Color (r mc) (g mc) (b mc) transparency)) : mask transparency [] masks
mask transparency ((bt, bs, bc) : bases) (masks) = (bt, bs, bc) : mask transparency bases masks 

colorPixel :: Point -> ColoredDrawing -> Color
colorPixel p drawing = computeSummativeColor p drawing transparent 

computeSummativeColor :: Point -> ColoredDrawing -> Color -> Color
computeSummativeColor p [] col = col
computeSummativeColor p ((t,s,c):ds) col = 
  if inside1 p (t, s) 
    then recursiveCall alphaBlend 
    else recursiveCall col 
    where
    recursiveCall = computeSummativeColor p ds
    alphaBlend = (Color (blendColor (r col) (r c) (a c)) (blendColor (g col) (g c) (a c)) (blendColor (b col) (b c) (a c)) 255)

blendColor :: Word8 -> Word8 -> Word8 -> Word8 
blendColor backgroundCol foregroundCol foregroundAlpha = fromIntegral $ (bc * (255 - fa) + fc * fa) `div` 255 
     where 
          bc = fromIntegral backgroundCol
          fc = fromIntegral foregroundCol
          fa = fromIntegral foregroundAlpha


inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
Vector x y `insides` Rectangle aspect = (sqrt (y**2) <= 1) && (sqrt (x**2) <= aspect)
Vector x y `insides` Ellipse aspect = (x**2/aspect**2) + (y**2/1**2) <= 1

Vector x y `insides` ConvexPolygon (p1:(p2:[])) = halfLine (Vector x y) p1 p2
Vector x y `insides` ConvexPolygon (p1:(p2:ps)) = if halfLine (Vector x y) p1 p2 then (Vector x y) `insides` ConvexPolygon (p2:ps) else False

halfLine :: (Vector) -> (Vector) -> (Vector) -> Bool
halfLine (Vector x y) (Vector x1 y1) (Vector x2 y2) = zcross (Vector (x1-x) (y1-y)) (Vector (x2-x1) (y2-y1)) > 0
                                                        where zcross (Vector a b) (Vector c d) = a*d - b*c

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)


-- Mandelbrot functions
next :: Point -> Point -> Point
next (Vector u v) (Vector x y) = Vector (x*x - y*y +u) (2*x*y+v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (Vector 0 0)

fairlyClose :: Point -> Bool
fairlyClose (Vector u v) = (u*u + v*v) < 100

inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

mandelbrotDrawing :: (Double, Double) -> (Double, Double) -> Drawing
mandelbrotDrawing (x1, y1) (x2, y2) = 
  [ 
    if (approxTest 100 (point x y)) then
    (scale (point 0.1 0.1) <+> translate (point x y), circle)
    else
      (translate (point 0 0), empty) | x <- [x1, x1 + 0.1 .. x2], y <- [y1, y1 + 0.1 ..y2]]

