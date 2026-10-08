module Geometry where

import System.Random

data Triangle = Triangle {
    -- sides
    a  :: Float
  , b  :: Float
  , c  :: Float

    -- angles (degrees)
  , al :: Float
  , bl :: Float
  , cl :: Float

  , s  :: Float

  , unknownX :: (Float, Float, Float)
} deriving (Show)

roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round (x * (10^n))) / (10.0^^n)

rnd :: (Float, Float) -> StdGen -> (Float, StdGen)
rnd range g = randomR range g

-- degrees ↔ radians conversion
toRad :: Float -> Float
toRad x = x * pi / 180

toDeg :: Float -> Float
toDeg x = x * 180 / pi

checkTriangleTask :: Triangle -> (Float, Float, Float) -> Bool
checkTriangleTask triangle (x1, x2, x3) =
  let
    (x1', x2', x3') = unknownX triangle
    x1T = (x1 > (x1'-1)) && (x1 < (x1'+1)) 
    x2T = (x2 > (x2'-1)) && (x2 < (x2'+1)) 
    x3T = (x3 > (x3'-1)) && (x3 < (x3'+1)) 
  in
    x1T && x2T && x3T

genTriangleTask :: StdGen -> Triangle
genTriangleTask g0 =
  let
    (typ, g1) = randomR (0 :: Int, 3) g0

    sideRange  = (3.0, 15.0)
    angleRange = (20.0, 100.0)

    go :: StdGen -> (Triangle, StdGen)
    go g = case typ of

      ----------------------------------------------------------------
      -- 0. SSS  (all sides known → find angles)
      ----------------------------------------------------------------
      0 ->
        let (a', g2) = rnd sideRange g
            (b', g3) = rnd sideRange g2
            (c', g4) = rnd sideRange g3
            valid = a' + b' > c' && a' + c' > b' && b' + c' > a'
        in if valid
           then
             let
               -- law of cosines (acos returns radians → convert to degrees)
               al' = toDeg $ acos $ (b'*b' + c'*c' - a'*a') / (2 * b' * c')
               bl' = toDeg $ acos $ (a'*a' + c'*c' - b'*b') / (2 * a' * c')
               cl' = toDeg $ acos $ (a'*a' + b'*b' - c'*c') / (2 * a' * b')
             in (Triangle a' b' c' 0 0 0 0 (al', bl', cl'), g4)
           else go g4

      ----------------------------------------------------------------
      -- 1. AAA  (all angles known → find sides)
      --        Pick one side randomly, compute the others via law of sines
      ----------------------------------------------------------------
      1 ->
        let (al', g2) = rnd angleRange g
            (bl', g3) = rnd angleRange g2
            cl' = 180 - al' - bl'
        in if cl' > 15 && cl' < 150
           then
             let
               (a', g4) = rnd sideRange g3          -- pick one side randomly
               b' = a' * sin (toRad bl') / sin (toRad al')
               c' = a' * sin (toRad cl') / sin (toRad al')
             in (Triangle 0 0 0 al' bl' cl' 0 (a', b', c'), g4)
           else go g3

      ----------------------------------------------------------------
      -- 2. SAS  (two sides + included angle)
      ----------------------------------------------------------------
      2 ->
        let (a', g2) = rnd sideRange g
            (b', g3) = rnd sideRange g2
            (cl', g4) = rnd angleRange g3          -- angle between sides a and b

            -- law of cosines
            c' = sqrt $ a'*a' + b'*b' - 2*a'*b' * cos (toRad cl')

            -- remaining angles
            al' = toDeg $ acos $ (b'*b' + c'*c' - a'*a') / (2 * b' * c')
            bl' = 180 - al' - cl'
        in (Triangle a' b' 0 0 0 cl' 0 (c', al', bl'), g4)

      ----------------------------------------------------------------
      -- 3. AAS / ASA  (side + two angles)
      ----------------------------------------------------------------
      3 ->
        let (a', g2) = rnd sideRange g
            (al', g3) = rnd angleRange g2
            (bl', g4) = rnd angleRange g3
            cl' = 180 - al' - bl'
        in if cl' > 15 && cl' < 150
           then
             let
               b' = a' * sin (toRad bl') / sin (toRad al')
               c' = a' * sin (toRad cl') / sin (toRad al')
             in (Triangle a' 0 0 al' bl' 0 0 (cl', b', c'), g4)
           else go g4

      _ -> go g1

    (tri, _) = go g1
  in tri

data Circle = Circle {
  radius :: Integer
  
  , square :: Float
  , lengthC :: Float
}
