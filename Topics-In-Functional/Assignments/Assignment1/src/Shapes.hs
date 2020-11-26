module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rectangle, ellipse,
  identity, translate, rotate, scale, (<+>),
  inside, 
  next, mandelbrot, fairlyClose, inMandelbrotSet, approxTest, mandelbrotDrawing)  where


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

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

data Shape = Empty 
           | Circle 
           | Square
           | Rectangle AspectRatio
           | Ellipse AspectRatio
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
rectangle width height = Rectangle (width/height)
ellipse width height = Ellipse (width/height)

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
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape)]

-- interpretation function for drawings

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