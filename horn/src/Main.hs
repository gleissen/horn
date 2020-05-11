module Main where
import qualified Horn.Bindings.Z3   as HZ3
import qualified Horn.Logic.Clauses as HC
import           Z3.Monad

main :: IO ()
main = do
  evalZ3 HZ3.test >>= \mbSol ->
        case mbSol of
          Nothing  -> putStr "No solution found."
          Just sol -> putStr "Solution: " >> print sol
