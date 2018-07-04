-- ================================== 2012 ===================================

-- 2. a)
type Point = (Double, Double)
data Object = Rectangle Point Double Double | Circle Point Double deriving Show
data Image = Image [Object] deriving Show

-- 2. b)
-- (Image [(Circle (0, 0) 5), (Rectangle (5, 0) 10 10), (Circle (15, 0) 5)])

-- 2. c)
getSquares :: Image -> [Object]
getSquares (Image os) = getSquares' os []
  where getSquares' [] l = l
        getSquares' ((Rectangle p w h):os) l = getSquares' os $ l ++ [(Rectangle p w h)]
        getSquares' (_:os) l = getSquares' os l

-- 2. d)
totalArea :: Image -> Double
totalArea (Image os) = totalArea' os 0
  where totalArea' [] f = f 
        totalArea' ((Rectangle _ w h):os) f = totalArea' os $ f + (w * h)
        totalArea' ((Circle _ r):os) f = totalArea' os $ f + (pi * (r * r))

-- 2. e)
scale :: Image -> Double -> Image
scale (Image os) f = Image $ scale' os f []
  where scale' [] f l = l
        scale' ((Rectangle (x, y) w h):os) f l = scale' os f 
                $ l ++ [Rectangle (x * f, y * f) (w * f) (h * f)]
        scale' ((Circle (x, y) r):os) f l = scale' os f
                $ l ++ [Circle (x * f, y * f) (r * f)]