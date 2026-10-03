module Main (main) where
import Equation(genOneRootEq)
import System.Random (newStdGen)

main :: IO ()
main = do
    g <- newStdGen
    putStrLn . show $ genOneRootEq g 1000 1000 (negate 1000) (negate 1000)
