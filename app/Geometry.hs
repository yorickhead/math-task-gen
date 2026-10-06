module Geometry where

import System.Random

data Triangle = Triangle {
    -- lines
    a :: Float
    , b :: Float
    , c :: Float

    -- angle
    , al :: Float
    , bl :: Float
    , cl :: Float

    , s :: Float

    , unknownX :: (Float, Float, Float)
} deriving (Show)

rnd :: (Float, Float) -> StdGen -> (Float, StdGen)
rnd range g = randomR range g

checkTriangleTask :: Triangle -> (Float, Float, Float) -> Bool
checkTriangleTask triangle unknowns = (unknownX triangle) == unknowns   

genTriangleTask :: StdGen -> Triangle
genTriangleTask g0 =
  let
    (typ, g1) = randomR (0 :: Int, 3) g0

    sideRange  = (3.0, 15.0)
    angleRange = (20.0, 100.0)

    go :: StdGen -> (Triangle, StdGen)
    go g = case typ of        

      -- 0. (SSS)
      0 ->
        let (a', g2) = rnd sideRange g
            (b', g3) = rnd sideRange g2
            (c', g4) = rnd sideRange g3
            valid = a' + b' > c' && a' + c' > b' && b' + c' > a'
        in if valid
           then 
            let
              al' = (((b'*b')+(c'*c')-(a'*a')) / (2 * b' * c'))
              bl' = (((a'*a')+(c'*c')-(b'*b')) / (2 * a' * c'))
              cl' = (((b'*b')+(a'*a')-(c'*c')) / (2 * b' * a'))

            in (Triangle a' b' c' 0 0 0 0 (
              acos . fromIntegral $ al', 
              acos . fromIntegral $ bl', 
              acos . fromIntegral $ cl'), g4)
           else go g4                    

      -- 1. (AAA)
      1 ->
        let (al', g2) = rnd angleRange g
            (bl', g3) = rnd angleRange g2
            cl' = 180 - al' - bl'
        in if cl' > 15 && cl' < 150
           then 
            let
              a' = fromIntegral ((b*b) + (c*c) - (2 * b * c))
              b' = fromIntegral ((a*a) + (c*c) - (2 * a * c))
              c' = fromIntegral ((a*a) + (b*b) - (2 * a * b))

            in (Triangle a' b' c' al' bl' cl' 0 (
              acos . fromIntegral $ al', 
              acos . fromIntegral $ bl', 
              acos . fromIntegral $ cl'), g3)
           else go g3

      -- 2. SS + A
      2 ->
        let (a', g2) = rnd sideRange g
            (b', g3) = rnd sideRange g2
            (al', g4) = rnd angleRange g3
        in (Triangle a' b' 0 al' 0 0 0, g4)

      -- 3. S + AA
      3 ->
        let (a', g2) = rnd sideRange g
            (al', g3) = rnd angleRange g2
            (bl', g4) = rnd angleRange g3
            cl' = 180 - al' - bl'
        in if cl' > 15 && cl' < 150
           then (Triangle a' 0 0 al' bl' cl' 0, g4)
           else go g4
      _ -> go g

    (tri, _) = go g1
  in tri