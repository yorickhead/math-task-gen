module Main (main) where
import Geometry (genTriangleTask)
import System.Random (newStdGen)

main :: IO ()
main = do
    g <- newStdGen
    putStrLn . show . genTriangleTask $ g