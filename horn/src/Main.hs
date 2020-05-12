module Main where
import qualified Horn.Bindings.Z3     as HZ3
import qualified Horn.Logic.Clauses   as HC
import qualified Horn.SymExec.SymExec as Sym
import           Z3.Monad

main :: IO ()
main = do
    HZ3.test >>= \mbSol ->
        case mbSol of
          Nothing  -> putStr "Unsat!"
          Just sol -> putStr "Solution: " >> print sol
    Sym.test
