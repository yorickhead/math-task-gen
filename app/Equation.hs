module Equation ( genOneRootEq ) where  

import System.Random                    

data Equation
  = OneRoot
      { k :: Integer
      , b :: Integer
      , x :: Integer
      }
  | TwoRoot
      { a  :: Integer
      , b  :: Integer
      , c  :: Integer
      , x1 :: Maybe Float
      , x2 :: Maybe Float
      }
  deriving (Show)

genOneRootEq :: StdGen -> Integer -> Integer -> Integer -> Integer -> Equation    
genOneRootEq g maxK maxB minK minB =
  let 
    (k', g1) = randomR (minK, maxK) g
    
    candidates = [y | y <- [minB..maxB], k' /= 0, mod (negate y) k' == 0]
    (b', _) = case candidates of
                [] -> error "нет подходящих кандидатов"
                xs -> let (i, g') = randomR (0, length xs - 1) g1
                      in (xs !! i, g')
    
    x' = (negate b') `div` k'
  in
    OneRoot k' b' x'

genSRootWithoutRootEq :: StdGen -> Integer -> Integer -> Integer -> Integer -> Equation
genSRootWithoutRootEq g coeffA coeffB maxA maxB = 
  let
    bound = max (abs maxA) (abs maxB)
    candidates = [cVal | cVal <- [negate (bound * bound) .. bound * bound]
                       , (coeffB*coeffB) - (4 * coeffA * cVal) < 0]
    (c', _) = case candidates of
                [] -> error "нет подходящих кандидатов"
                xs -> let (i, g') = randomR (0, length xs - 1) g
                      in (xs !! i, g')
  in
    TwoRoot coeffA coeffB c' Nothing Nothing

genSRootWithOneRootEq :: Integer -> Integer -> Equation
genSRootWithOneRootEq coeffA coeffB =
  let 
    c'  = (coeffB * coeffB) `div` (4 * coeffA)
    x1' = fromIntegral (negate coeffB) / fromIntegral (2 * coeffA)
  in
    TwoRoot coeffA coeffB c' (Just x1') Nothing 

genSRootWithTwoRootEq :: StdGen -> Integer -> Integer -> Integer -> Integer -> Equation
genSRootWithTwoRootEq g coeffA coeffB maxA maxB =
  let
    candidates =
      [ x'
      | x' <- map (^2) [1 .. (abs maxA + abs maxB)]
      , ((coeffB * coeffB) - x') `mod` (4 * coeffA) == 0
      ]

    (d, _) = case candidates of
      [] -> error "нет подходящих кандидатов"
      xs -> let (i, g') = randomR (0, length xs - 1) g
            in (xs !! i, g')

    coeffC = ((coeffB * coeffB) - d) `div` (4 * coeffA)

    sqrtD = sqrt (fromIntegral d)
    x1'   = (fromIntegral (negate coeffB) - sqrtD) / fromIntegral (2 * coeffA)
    x2'   = (fromIntegral (negate coeffB) + sqrtD) / fromIntegral (2 * coeffA)
  in
    TwoRoot coeffA coeffB coeffC (Just x1') (Just x2') 


genSecondRootEq :: StdGen -> Integer -> Integer -> Integer -> Integer -> Equation
genSecondRootEq g maxA maxB minA minB =
  let 
    (a', g1) = randomR (minA, maxA) g
    (b', g2) = randomR (minB, maxB) g1
    (haveX, g3) = randomR (1, 7) g2 :: (Int, StdGen)
  in
    case haveX of
      0 -> genSRootWithoutRootEq g3 a' b' maxA maxB
      1 -> genSRootWithOneRootEq  a' b'
      otherwise -> genSRootWithTwoRootEq g3 a' b' maxA maxB