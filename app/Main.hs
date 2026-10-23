module Main (main) where
import Equation (equationToTask, genSecondRootEq)
import System.Random (newStdGen)

main :: IO ()
main = do
    g <- newStdGen
    putStrLn . equationToTask $ (genSecondRootEq g 5 (-40, 40) (-40, 40) (-40, 40))