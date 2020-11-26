module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, rectangle, ellipse, convexPolygon,
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

Vector x y `insides` ConvexPolygon (p1:(p2:[])) = halfLine (Vector x y) p1 p2
Vector x y `insides` ConvexPolygon (p1:(p2:ps)) = if halfLine (Vector x y) p1 p2 then (Vector x y) `insides` ConvexPolygon (p2:ps) else False

halfLine (Vector x y) (Vector x1 y1) (Vector x2 y2) = zcross (Vector (x1-x) (y1-y)) (Vector (x2-x1) (y2-y1)) > 0
                                                        where zcross (Vector a b) (Vector c d) = a*d - b*c
{-
Vector x y `insides` ConvexPolygon ps = helper (Vector x y) (ConvexPolygon ps) 0 0 (length ps)

helper :: Point -> Shape -> Int -> Int -> Int -> Bool
helper (Vector x y) (ConvexPolygon ((Vector x1 y1) : ((Vector x2 y2) : ps))) i count n =
    if isIntersect (Vector x1 y1) (Vector x2 y2) (Vector x y) (Vector 9999 y) 
    && direction (Vector x1 y1) (Vector x y) (Vector x2 y2) == 0
          then onLine (Vector x1 y1) (Vector x2 y2) (Vector x y)
        else 
        let countplus = count + 1
            iplus = (i+1) `mod` n in
            if i /= 0
              then helper (Vector x y) (ConvexPolygon (Vector x2 y2 : ps)) iplus countplus n
              else (countplus `mod` 1) == 0
{-
Vector x y `insides` ConvexPolygon [] = False
Vector x y `insides` ConvexPolygon (Vector x1 y1 : []) = False
Vector x y `insides` ConvexPolygon (Vector x1 y1 : (Vector x2 y2 : ps)) = 
  if isIntersect (Vector x1 y1) (Vector x2 y2) (Vector x y) (Vector 9999 y)
    && direction (Vector x1 y1) (Vector x y) (Vector x2 y2) == 0
      then onLine (Vector x1 y1) (Vector x2 y2) (Vector x y)
      else (Vector x2 y2 `insides` ConvexPolygon ps)
-}      

{-
isIntersect (Vector line1point1x1 line1point1y1) (Vector line1point1x2 line1point1y2) 
            (Vector line1point2x1 line1point2y1) (Vector line1point2x2 line1point2y2)
            (Vector line2point1x1 line2point1y1) (Vector line2point1x2 line2point1y2)
            (Vector line2point2x1 line2point2y1) (Vector line2point2x2 line2point2y2) = 
-}

onLine :: (Vector) -> (Vector) -> (Vector) -> Bool
onLine (Vector x1 y1) (Vector x2 y2) (Vector px py) = 
  if px <= (max x1 x2) && px <= (min x1 x2) && py <= (max y1 y2) && py <= (min y1 y2)
    then True
    else False

isIntersect :: (Vector) -> (Vector) -> (Vector) -> (Vector) -> Bool
isIntersect line1point1 line1point2 line2point1 line2point2 =
  let dir1 = direction line1point1 line1point2 line2point1
      dir2 = direction line1point1 line1point2 line2point2
      dir3 = direction line2point1 line2point2 line1point1
      dir4 = direction line2point1 line2point2 line1point2
  in if (dir1 /= dir2 && dir3 /= dir4) -- they are intersecting
    || (dir1 == 0 && onLine line1point1 line1point2 line2point1) -- when p2 of line2 is on line1
    || (dir2 == 0 && onLine line1point1 line1point2 line2point2) -- when p1 of line2 is on line 1
    || (dir3 == 0 && onLine line2point1 line2point2 line1point1) -- when p2 of line1 is on line 2
    || (dir4 == 0 && onLine line2point1 line2point2 line1point2) -- when p1 of line1 is on line2
    then True 
    else False

direction :: (Vector) -> (Vector) -> (Vector) -> Int
direction (Vector ax ay) (Vector bx by) (Vector cx cy) = 
  if val == 0
    then 0    -- colinear
    else if val < 0
      then 2 -- anti-clockwise direction
      else 1 -- clockwise direction
      where val = (by-ay) * (cx-bx) - (bx-ax)*(cy-by)
-}

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